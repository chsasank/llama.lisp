import numpy as np

N = 256

spec = {
    "name": "matmul",
    "kernel": "matmul.sexp",
    "compile": {"cuda_lisp": False, "mcpu": "sm_86", "llc_o": 0},
    "grid": [(N + 31) // 32, (N + 31) // 32, 1],
    "block": [32, 32, 1],
    "buffers": [
        {"name": "a", "shape": [N, N], "dtype": "float32", "init": "random"},
        {"name": "b", "shape": [N, N], "dtype": "float32", "init": "random"},
        {"name": "c", "shape": [N, N], "dtype": "float32", "init": "zero", "output": True},
    ],
    "args": ["a", "b", "c", N],
    "verify": {
        "type": "max_error",
        "tolerance": 1e-3,
        "expected": lambda arrays: arrays["a"] @ arrays["b"],
    },
}
