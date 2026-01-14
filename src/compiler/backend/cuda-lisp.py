import importlib
import json
import os
import sys


class CodegenError(Exception):
    pass


class CudaLisp:
    def __init__(self):
        self.global_exprs = []
        self.intrinsics_added = []

    def compile_shared(self, expr):
        arr_name = expr[1]
        arr_type = expr[2]
        return ["define-global", [arr_name, ["arr", *arr_type]], ["addrspace", 3]]

    def is_symbol(self, expr):
        return isinstance(expr, str)

    def is_list(self, expr):
        return isinstance(expr, list)

    def is_ptr_type(self, typ):
        return isinstance(typ, list) and typ[0] == "ptr"

    def is_arr_type(self, typ):
        return isinstance(typ, list) and typ[0] == "arr"

    def compile_intrinsic_symbols(self, expr):
        symbol_intr_map = {
            # threadId.x
            "tid.x": ["llvm.nvvm.read.ptx.sreg.tid.x", "int"],
            "tid.y": ["llvm.nvvm.read.ptx.sreg.tid.y", "int"],
            "tid.z": ["llvm.nvvm.read.ptx.sreg.tid.z", "int"],
            # blockIdx.x
            "bid.x": ["llvm.nvvm.read.ptx.sreg.ctaid.x", "int"],
            "bid.y": ["llvm.nvvm.read.ptx.sreg.ctaid.y", "int"],
            "bid.z": ["llvm.nvvm.read.ptx.sreg.ctaid.z", "int"],
            # blockDim.x
            "bdim.x": ["llvm.nvvm.read.ptx.sreg.ntid.x", "int"],
            "bdim.y": ["llvm.nvvm.read.ptx.sreg.ntid.y", "int"],
            "bdim.z": ["llvm.nvvm.read.ptx.sreg.ntid.z", "int"],
            # gridDim.x
            "gdim.x": ["llvm.nvvm.read.ptx.sreg.nctaid.x", "int"],
            "gdim.y": ["llvm.nvvm.read.ptx.sreg.nctaid.y", "int"],
            "gdim.z": ["llvm.nvvm.read.ptx.sreg.nctaid.z", "int"],
            # __syncthreads
            "__syncthreads": ["llvm.nvvm.barrier0", "void"],
        }

        if isinstance(expr, list):
            if len(expr) == 1 and self.is_symbol(expr[0]):
                intr = expr[0]
                if intr in symbol_intr_map:
                    # add definition to global_exprs if not alreday
                    if expr not in self.intrinsics_added:
                        self.global_exprs.append(["define", [[*symbol_intr_map[intr]]]])
                        self.intrinsics_added.append(intr)

                    return ["call", symbol_intr_map[intr][0]]
                else:
                    return expr
            else:
                return [self.compile_intrinsic_symbols(x) for x in expr]
        else:
            return expr

    def compile_macros(self, expr):
        def declare_global(expr):
            if len(expr) != 3:
                raise CodegenError(f"invalid declare-global: {expr}")

            name = expr[1]
            typ = expr[2]
            if self.is_ptr_type(typ):
                typ.append(["addrspace", 1])
            else:
                raise CodegenError(f"declare-global has to be a pointer: {expr}")

            return ["declare", name, typ]

        macros = {
            "declare-global": declare_global,
        }

        if self.is_list(expr):
            if self.is_symbol(expr[0]) and expr[0] in macros:
                return macros[expr[0]](expr)
            else:
                return [self.compile_macros(x) for x in expr]
        else:
            return expr

    def compile_kernel(self, expr):
        if len(expr) < 2:
            raise CodegenError(f"Invalid define-kernel: {expr}")

        fn_proto = expr[1]
        fn_ret = fn_proto[0]
<<<<<<< HEAD
        for fn_arg in fn_proto[1:]:
            fn_arg_type = fn_arg[1]
            # arg pointers have addrspace 1 by default
            if self.is_ptr_type(fn_arg_type) and len(fn_arg_type) == 2:
                fn_arg_type.append(["addrspace", 1])
=======
>>>>>>> 3b9315452d68b2f947b1f3dc387ba727951a3012

        # expand all intrinsics
        fn_body = self.compile_intrinsic_symbols(expr[2:])

        # expand all macros
        fn_body = self.compile_macros(fn_body)
        return ["define", fn_proto, *fn_body]

    def preprocess(self, expr):
        assert expr[0] == "cuda-lisp"

        global_exprs = expr[1:]
        for global_expr in global_exprs:
            if global_expr[0] == "define-shared":
                # compile shared
                self.global_exprs.append(self.compile_shared(global_expr))
            elif global_expr[0] == "define-kernel":
                self.global_exprs.append(self.compile_kernel(global_expr))
            else:
                # recursively check for symbols
                self.global_exprs.append(global_expr)

        return ["c-lisp", *self.global_exprs]


def main():
    cuda = CudaLisp()
    expr = json.load(sys.stdin)
    print(json.dumps(cuda.preprocess(expr)))


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="What the program does")
    args = parser.parse_args()
    main()
