import numpy as np

N = 64

spec = {
    "name": "vec_copy",
    "kernel": "kernel.sexp",
    "compile": {
        "cuda_lisp": True,
        "opt_passes": "strip,mem2reg,simplifycfg,always-inline",
        "opt_o": 3,
        "mcpu": "sm_86",
        "llc_o": 0,
    },
    "grid": [1, 1, 1],
    "block": [N, 1, 1],
    "buffers": [
        {"name": "src", "shape": [N], "dtype": "int32", "init": "arange"},
        {"name": "dst", "shape": [N], "dtype": "int32", "init": "zero", "output": True},
    ],
    "args": ["src", "dst", N],
    "verify": {"type": "exact", "expected": lambda arrays: arrays["src"]},
}
