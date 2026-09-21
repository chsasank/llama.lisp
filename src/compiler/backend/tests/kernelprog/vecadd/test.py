import numpy as np

N = 32

spec = {
    "name": "vecadd",
    "kernel": "vecadd.sexp",
    "compile": {"cuda_lisp": False, "mcpu": "sm_75", "llc_o": 0},
    "grid": [1, 1, 1],
    "block": [N, 1, 1],
    "buffers": [
        {"name": "a", "shape": [N], "dtype": "float32", "init": "random"},
        {"name": "b", "shape": [N], "dtype": "float32", "init": "random"},
        {"name": "c", "shape": [N], "dtype": "float32", "init": "zero", "output": True},
    ],
    "args": ["a", "b", "c"],
    "verify": {
        "type": "max_error",
        "tolerance": 1e-5,
        "expected": lambda arrays: arrays["a"] + arrays["b"],
    },
}
