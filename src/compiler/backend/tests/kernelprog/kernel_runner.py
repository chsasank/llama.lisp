#!/usr/bin/env python3
"""Generic CUDA kernel test runner.

Each test directory contains:
  kernel.sexp    -- kernel source
  test.json      -- test specification
  test.expected  -- expected stdout (optional; if absent, just exit code matters)

The runner compiles kernel.sexp to PTX, executes it on the GPU, and verifies
outputs with NumPy reference expressions.
"""

import argparse
import ctypes
import os
import re
import runpy
import subprocess
import sys
import tempfile

import numpy as np


# ---------------------------------------------------------------------------
# CUDA driver API bindings (no external dependencies beyond NumPy + libcuda.so)
# ---------------------------------------------------------------------------

try:
    libcuda = ctypes.CDLL("libcuda.so", mode=ctypes.RTLD_GLOBAL)
except OSError:
    libcuda = ctypes.CDLL("libcuda.so.1", mode=ctypes.RTLD_GLOBAL)

CUresult = ctypes.c_int
CUdevice = ctypes.c_int
CUcontext = ctypes.c_void_p
CUmodule = ctypes.c_void_p
CUfunction = ctypes.c_void_p
CUdeviceptr = ctypes.c_uint64
CUstream = ctypes.c_void_p

CUDA_SUCCESS = 0


def _bind(name, argtypes, restype=CUresult):
    fn = getattr(libcuda, name)
    fn.argtypes = argtypes
    fn.restype = restype
    return fn


cuInit = _bind("cuInit", [ctypes.c_uint])
cuDeviceGet = _bind("cuDeviceGet", [ctypes.POINTER(CUdevice), ctypes.c_int])
cuCtxCreate = _bind("cuCtxCreate", [ctypes.POINTER(CUcontext), ctypes.c_void_p,
                                    ctypes.c_uint, CUdevice])
cuModuleLoadData = _bind("cuModuleLoadData", [ctypes.POINTER(CUmodule), ctypes.c_char_p])
cuModuleGetFunction = _bind("cuModuleGetFunction", [ctypes.POINTER(CUfunction),
                                                    CUmodule, ctypes.c_char_p])
cuMemAlloc = _bind("cuMemAlloc", [ctypes.POINTER(CUdeviceptr), ctypes.c_size_t])
cuMemFree = _bind("cuMemFree", [CUdeviceptr])
cuMemcpyHtoD = _bind("cuMemcpyHtoD", [CUdeviceptr, ctypes.c_void_p, ctypes.c_size_t])
cuMemcpyDtoH = _bind("cuMemcpyDtoH", [ctypes.c_void_p, CUdeviceptr, ctypes.c_size_t])
cuLaunchKernel = _bind("cuLaunchKernel",
                       [CUfunction,
                        ctypes.c_uint, ctypes.c_uint, ctypes.c_uint,
                        ctypes.c_uint, ctypes.c_uint, ctypes.c_uint,
                        ctypes.c_uint, CUstream,
                        ctypes.c_void_p, ctypes.c_void_p])
cuCtxSynchronize = _bind("cuCtxSynchronize", [])
cuCtxDestroy = _bind("cuCtxDestroy", [CUcontext])
cuModuleUnload = _bind("cuModuleUnload", [CUmodule])


def cu_check(res, msg=""):
    if res != CUDA_SUCCESS:
        err = ctypes.c_char_p()
        libcuda.cuGetErrorString(res, ctypes.byref(err))
        raise RuntimeError(f"CUDA error{(': ' + msg) if msg else ''}: {err.value.decode()}")


# ---------------------------------------------------------------------------
# Compilation helpers
# ---------------------------------------------------------------------------

DATALAYOUT = (
    "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-"
    "f32:32:32-f64:64:64-v16:16:16-v32:32:32-v64:64:64-v128:128:128-n16:32:64"
)
TRIPLE = "nvptx64-nvidia-cuda"


def backend_dir():
    """Return absolute path to .../src/compiler/backend."""
    here = os.path.dirname(os.path.abspath(__file__))
    return os.path.abspath(os.path.join(here, "..", ".."))


def run_cmd(cmd, cwd=None, input_text=None):
    """Run a shell command and return stdout; raise on error."""
    proc = subprocess.run(
        cmd,
        cwd=cwd,
        input=input_text,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        check=False,
    )
    if proc.returncode != 0:
        raise RuntimeError(
            f"Command failed ({proc.returncode}): {' '.join(cmd)}\n"
            f"stderr: {proc.stderr}\nstdout: {proc.stdout}"
        )
    return proc.stdout


def extract_kernel_signature(ll_text):
    """Extract the kernel function type from LLVM IR for the NVVM annotation."""
    m = re.search(r"define\s+(\S+)\s+@\"?kernel\"?\s*\(", ll_text)
    if not m:
        raise RuntimeError("Could not find 'define @kernel' in generated LLVM IR")
    ret_type = m.group(1)

    # Find the matching closing parenthesis, skipping nested pairs.
    start = m.end()
    depth = 1
    end = start
    while end < len(ll_text) and depth > 0:
        c = ll_text[end]
        if c == '(':
            depth += 1
        elif c == ')':
            depth -= 1
        end += 1
    if depth != 0:
        raise RuntimeError("Unbalanced parentheses in kernel definition")
    params = ll_text[start:end - 1].strip()

    # Strip parameter names like %0, %"x" and attributes like nocapture readonly.
    params = re.sub(r'%["a-zA-Z0-9_.]+', "", params)
    # Remove common LLVM parameter attributes from function-type metadata.
    attrs = ["nocapture", "readonly", "writeonly", "noundef", "noalias",
             "nonnull", "dereferenceable", "dereferenceable_or_null", "byval",
             "zeroext", "signext", "inreg", "sret"]
    for attr in attrs:
        params = re.sub(rf"\b{attr}\b(?:\s*\([^)]*\))?", "", params)
    params = re.sub(r"\s+", " ", params).strip()
    params = re.sub(r",\s+", ", ", params)
    # Clean up stray commas before closing paren.
    params = re.sub(r",\s*$", "", params)
    return f"{ret_type} ({params})*"


def append_nvvm_annotations(ll_text, sig):
    """Append the NVVM kernel annotation metadata."""
    return (
        ll_text.rstrip()
        + "\n"
        + "!nvvm.annotations = !{!1}\n"
        + f'!1 = !{{{sig} @kernel, !"kernel", i32 1}}\n'
    )


def compile_kernel(sexp_path, compile_spec, out_dir):
    """Compile kernel.sexp to PTX; return path to PTX file."""
    base = os.path.splitext(os.path.basename(sexp_path))[0]
    ll_path = os.path.join(out_dir, f"{base}.ll")
    ptx_path = os.path.join(out_dir, f"{base}.ptx")

    be = backend_dir()
    utils_dir = os.path.join(be, "utils")

    # Stage 1: sexp -> json
    with open(sexp_path, "r") as f:
        sexp_text = f.read()
    json_text = run_cmd(["guile", os.path.join(utils_dir, "sexp-json.scm")],
                        input_text=sexp_text)

    # Stage 2: optional cuda-lisp preprocessor
    if compile_spec.get("cuda_lisp", False):
        json_text = run_cmd(["python", os.path.join(be, "cuda-lisp.py")],
                            input_text=json_text)

    # Stage 3: c-lisp
    json_text = run_cmd(["python", os.path.join(be, "c-lisp.py")],
                        input_text=json_text)

    # Stage 4: brilisp
    json_text = run_cmd(["python", os.path.join(be, "brilisp.py")],
                        input_text=json_text)

    # Stage 5: llvm
    ll_text = run_cmd(["python", os.path.join(be, "llvm.py")],
                      input_text=json_text)

    # Stage 6: optional opt passes
    opt_passes = compile_spec.get("opt_passes")
    if opt_passes:
        opt_o = compile_spec.get("opt_o", 3)
        ll_text = run_cmd(
            ["opt", "-p", opt_passes, "-S"],
            input_text=ll_text,
        )
        ll_text = run_cmd(
            ["opt", f"-O{opt_o}", "-S"],
            input_text=ll_text,
        )

    # Stage 7: fix target and annotate kernel
    ll_text = re.sub(r"^target datalayout.*", f'target datalayout = "{DATALAYOUT}"',
                     ll_text, flags=re.MULTILINE)
    ll_text = re.sub(r"^target triple.*", f'target triple = "{TRIPLE}"',
                     ll_text, flags=re.MULTILINE)
    sig = extract_kernel_signature(ll_text)
    ll_text = append_nvvm_annotations(ll_text, sig)

    with open(ll_path, "w") as f:
        f.write(ll_text)

    # Stage 8: llc -> PTX
    mcpu = compile_spec.get("mcpu", "sm_86")
    opt_level = compile_spec.get("llc_o", 0)
    run_cmd(["llc", f"-mcpu={mcpu}", f"-O{opt_level}", "-o", ptx_path, ll_path])

    return ptx_path


# ---------------------------------------------------------------------------
# Test execution
# ---------------------------------------------------------------------------

class KernelTest:
    def __init__(self, spec_path):
        self.spec_path = os.path.abspath(spec_path)
        self.test_dir = os.path.dirname(self.spec_path)
        # Load a Python test spec. It must define a top-level `spec` dict.
        module = runpy.run_path(self.spec_path)
        if "spec" not in module:
            raise RuntimeError(f"{self.spec_path} must define a top-level `spec` dict")
        self.spec = module["spec"]

    def _resolve_scalar(self, val, arrays):
        """Resolve a scalar spec value (int/float or $name.size expression)."""
        if isinstance(val, (int, float)):
            return val
        if isinstance(val, str) and val.startswith("$"):
            parts = val[1:].split(".")
            arr = arrays[parts[0]]
            if len(parts) == 1 or parts[1] == "size":
                return arr.size
            if parts[1] == "shape":
                idx = int(parts[2]) if len(parts) > 2 else 0
                return arr.shape[idx]
            raise ValueError(f"Cannot resolve scalar: {val}")
        raise ValueError(f"Bad scalar value: {val}")

    def _init_buffer(self, arr, init):
        if callable(init):
            ret = init(arr)
            if ret is not None:
                arr[:] = ret
        elif init == "zero":
            arr.fill(0)
        elif init == "ones":
            arr.fill(1)
        elif init == "arange":
            arr.flat[:] = np.arange(arr.size, dtype=arr.dtype).reshape(arr.shape)
        elif init == "random":
            if np.issubdtype(arr.dtype, np.floating):
                arr[:] = np.random.rand(*arr.shape).astype(arr.dtype)
            else:
                arr[:] = np.random.randint(0, 100, size=arr.shape, dtype=arr.dtype)
        elif init == "iota":
            arr.flat[:] = np.arange(1, arr.size + 1, dtype=arr.dtype).reshape(arr.shape)
        elif init == "constant":
            arr.fill(0)  # placeholder; overridden by "value" field
        elif isinstance(init, str) and init.startswith("np."):
            arr[:] = eval(init, {"np": np, "__builtins__": {}})
        elif isinstance(init, list):
            arr[:] = np.array(init, dtype=arr.dtype).reshape(arr.shape)
        else:
            raise ValueError(f"Unknown init: {init}")

        # Optional constant value override
        if "value" in self._current_buf_spec:
            arr.fill(self._current_buf_spec["value"])

    def run(self):
        spec = self.spec
        compile_cfg = spec.get("compile", {})
        sexp_file = os.path.join(self.test_dir, spec.get("kernel", "kernel.sexp"))

        with tempfile.TemporaryDirectory() as tmpdir:
            ptx_path = compile_kernel(sexp_file, compile_cfg, tmpdir)

            # Initialize CUDA
            cu_check(cuInit(0), "cuInit")
            device = CUdevice()
            cu_check(cuDeviceGet(ctypes.byref(device), 0), "cuDeviceGet")
            context = CUcontext()
            cu_check(cuCtxCreate(ctypes.byref(context), None, 0, device), "cuCtxCreate")

            try:
                # Load PTX
                module = CUmodule()
                with open(ptx_path, "rb") as f:
                    ptx_bytes = f.read()
                cu_check(cuModuleLoadData(ctypes.byref(module), ptx_bytes), "cuModuleLoadData")

                kernel = CUfunction()
                cu_check(cuModuleGetFunction(ctypes.byref(kernel), module, b"kernel"),
                         "cuModuleGetFunction")

                # Allocate and initialize host/device buffers
                arrays = {}
                dev_ptrs = {}
                output_names = []
                for buf_spec in spec["buffers"]:
                    self._current_buf_spec = buf_spec
                    name = buf_spec["name"]
                    shape = tuple(buf_spec["shape"])
                    dtype = np.dtype(buf_spec["dtype"])
                    arr = np.empty(shape, dtype=dtype)
                    self._init_buffer(arr, buf_spec.get("init", "zero"))
                    arrays[name] = arr

                    dptr = CUdeviceptr()
                    cu_check(cuMemAlloc(ctypes.byref(dptr), arr.nbytes), f"cuMemAlloc {name}")
                    dev_ptrs[name] = dptr

                    if buf_spec.get("inout", False):
                        cu_check(cuMemcpyHtoD(dptr, arr.ctypes.data, arr.nbytes),
                                 f"cuMemcpyHtoD {name}")
                        output_names.append(name)
                    elif not buf_spec.get("output", False):
                        cu_check(cuMemcpyHtoD(dptr, arr.ctypes.data, arr.nbytes),
                                 f"cuMemcpyHtoD {name}")
                    else:
                        output_names.append(name)

                # Snapshot original host values before the kernel overwrites outputs.
                originals = {name: arr.copy() for name, arr in arrays.items()}

                # Build kernel argument array
                arg_specs = spec["args"]
                scalar_holders = []
                arg_ptr_list = []
                for a in arg_specs:
                    if isinstance(a, str) and a in arrays:
                        # pointer to CUdeviceptr value
                        holder = ctypes.c_uint64(dev_ptrs[a].value)
                        scalar_holders.append(holder)
                        arg_ptr_list.append(ctypes.cast(ctypes.pointer(holder), ctypes.c_void_p))
                    else:
                        scalar_val = self._resolve_scalar(a, arrays)
                        if isinstance(scalar_val, int):
                            holder = ctypes.c_int32(scalar_val)
                        elif isinstance(scalar_val, float):
                            holder = ctypes.c_float(scalar_val)
                        else:
                            raise TypeError(f"Unsupported scalar type: {type(scalar_val)}")
                        scalar_holders.append(holder)
                        arg_ptr_list.append(ctypes.cast(ctypes.pointer(holder), ctypes.c_void_p))

                arg_array = (ctypes.c_void_p * len(arg_ptr_list))(*arg_ptr_list)

                grid = spec.get("grid", [1, 1, 1])
                block = spec.get("block", [1, 1, 1])
                shared = spec.get("shared_mem", 0)

                cu_check(
                    cuLaunchKernel(
                        kernel,
                        grid[0], grid[1], grid[2],
                        block[0], block[1], block[2],
                        shared, 0,
                        ctypes.cast(arg_array, ctypes.c_void_p),
                        None,
                    ),
                    "cuLaunchKernel",
                )
                cu_check(cuCtxSynchronize(), "cuCtxSynchronize")

                # Copy outputs back
                for name in output_names:
                    arr = arrays[name]
                    cu_check(cuMemcpyDtoH(arr.ctypes.data, dev_ptrs[name], arr.nbytes),
                             f"cuMemcpyDtoH {name}")

                # Verify
                ok = self._verify(arrays, originals, output_names)

                # Cleanup device memory
                for dptr in dev_ptrs.values():
                    cu_check(cuMemFree(dptr), "cuMemFree")
                cu_check(cuModuleUnload(module), "cuModuleUnload")

                return ok
            finally:
                cu_check(cuCtxDestroy(context), "cuCtxDestroy")

    def _verify(self, arrays, originals, output_names):
        spec = self.spec
        verify = spec.get("verify", {})
        vtype = verify.get("type", "exact")
        tol = verify.get("tolerance", 1e-5)

        # Build namespace for eval (use originals so inout inputs are not overwritten).
        ns = {"np": np}
        ns.update(originals)

        # Expected reference expression
        expected_expr = verify.get("expected", None)
        if expected_expr is None:
            # Default: for single output, expected is the first input of the same shape
            inputs = [n for n, b in zip(
                [bs["name"] for bs in spec["buffers"]],
                spec["buffers"]
            ) if not b.get("output", False)]
            if len(output_names) == 1 and inputs:
                expected_expr = inputs[0]
            else:
                raise ValueError("No expected expression provided and cannot infer one")

        if callable(expected_expr):
            expected = expected_expr(originals)
        elif isinstance(expected_expr, str):
            expected = eval(expected_expr, {"__builtins__": {}}, ns)
        else:
            expected = expected_expr
        if not isinstance(expected, np.ndarray):
            expected = np.array(expected)

        results = []
        for out_name in output_names:
            actual = arrays[out_name]
            if vtype == "exact":
                ok = np.array_equal(actual, expected)
            elif vtype == "max_error":
                err = np.max(np.abs(actual.astype(np.float64) - expected.astype(np.float64)))
                ok = err <= tol
                if not ok:
                    print(f"max_error = {err} (tolerance {tol})")
            elif vtype == "allclose":
                ok = np.allclose(actual, expected, atol=tol)
            else:
                raise ValueError(f"Unknown verify type: {vtype}")
            results.append((out_name, ok))

        all_ok = all(ok for _, ok in results)
        if all_ok:
            print(f"{spec.get('name', self.test_dir)}: PASSED")
        else:
            print(f"{spec.get('name', self.test_dir)}: FAILED")
            for name, ok in results:
                print(f"  {name}: {'PASSED' if ok else 'FAILED'}")
        return all_ok


def main():
    parser = argparse.ArgumentParser(description="Run a kernelprog GPU test from a Python spec.")
    parser.add_argument("spec", help="Path to test.py")
    args = parser.parse_args()

    test = KernelTest(args.spec)
    ok = test.run()
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
