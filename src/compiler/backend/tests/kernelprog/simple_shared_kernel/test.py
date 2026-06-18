import numpy as np

N = 64

spec = {
    "name": "simple_shared_kernel",
    "kernel": "kernel.sexp",
    "compile": {"cuda_lisp": False, "mcpu": "sm_86", "llc_o": 0},
    "grid": [1, 1, 1],
    "block": [N, 1, 1],
    "buffers": [
        {"name": "d", "shape": [N], "dtype": "int32", "init": "arange", "inout": True},
    ],
    "args": ["d", N],
    "verify": {
        "type": "exact",
        "expected": lambda arrays: np.arange(N - 1, -1, -1, dtype=np.int32),
    },
}
