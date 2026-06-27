import numpy as np

N = 64

spec = {
    "name": "dynamic_shared_mem",
    "kernel": "kernel.sexp",
    "compile": {"cuda_lisp": False, "mcpu": "sm_86", "llc_o": 0},
    "grid": [1, 1, 1],
    "block": [N, 1, 1],
    "shared_mem": N * 4,
    "buffers": [
        {"name": "inp", "shape": [N], "dtype": "float32", "init": "arange"},
        {"name": "out", "shape": [N], "dtype": "float32", "init": "zero", "output": True},
    ],
    "args": [N, "out", "inp"],
    "verify": {
        "type": "exact",
        "expected": "inp",
    },
}
