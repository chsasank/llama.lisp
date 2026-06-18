import numpy as np

batch_size = 2048
num_elements_per_batch = 1024 * 256
total_elements = batch_size * num_elements_per_batch

spec = {
    "name": "reduction_vec_sum2",
    "kernel": "kernel.sexp",
    "compile": {"cuda_lisp": False, "mcpu": "sm_86", "llc_o": 0},
    "grid": [batch_size, 1, 1],
    "block": [256, 1, 1],
    "buffers": [
        {"name": "input", "shape": [total_elements], "dtype": "float32", "init": "ones"},
        {"name": "output", "shape": [batch_size], "dtype": "float32", "init": "zero", "output": True},
    ],
    "args": ["output", "input", num_elements_per_batch],
    "verify": {
        "type": "max_error",
        "tolerance": 1e-4,
        "expected": lambda arrays: np.sum(
            arrays["input"].reshape(batch_size, num_elements_per_batch), axis=1
        ),
    },
}
