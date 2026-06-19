import numpy as np

M = 256
N = 256
K = 256
alpha = 0.5
beta = 1.0


def random_small(arr):
    """Match the original driver's randomization: small signed values."""
    rng = np.random.default_rng(0)
    arr[:] = (rng.integers(0, 5, size=arr.shape) +
              0.01 * rng.integers(0, 5, size=arr.shape)).astype(arr.dtype)
    sign = rng.integers(0, 2, size=arr.shape).astype(bool)
    arr[sign] = -arr[sign]


spec = {
    "name": "matmul_shared_mem",
    "kernel": "kernel.sexp",
    "compile": {
        "cuda_lisp": False,
        "opt_passes": "strip,mem2reg,simplifycfg",
        "mcpu": "sm_86",
        "llc_o": 3,
    },
    "grid": [(M + 31) // 32, (N + 31) // 32, 1],
    "block": [32 * 32, 1, 1],
    "buffers": [
        {"name": "A", "shape": [M, K], "dtype": "float32", "init": random_small},
        {"name": "B", "shape": [K, N], "dtype": "float32", "init": random_small},
        {"name": "C", "shape": [M, N], "dtype": "float32", "init": random_small, "inout": True},
    ],
    "args": [M, N, K, alpha, "A", "B", beta, "C"],
    "verify": {
        "type": "max_error",
        "tolerance": 1e-2,
        "expected": lambda arrays: alpha * (arrays["A"] @ arrays["B"]) + beta * arrays["C"],
    },
}
