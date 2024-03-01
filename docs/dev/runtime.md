# Runtime

This language needs to feel like interpreted language when it really is a compiled language. That is how SBCL works as well. So JIT compilation is an important design goal.

## LLVM Lite?

Because I have figured out using python libraries [seamlessly](./python-interop.md) in common lisp, should I just use `llvmlite` library from numba[1]? It has a good runtime *and* llvm builder API. It generates LLVM as a text file meaning I can actually move from it to common lisp based generator once mature.

Let's look at number of lines builder API for llvmlite has. Alright it's a lot of lines: builder.py (the interface) alone has > 1000 lines! Not worth re-implementing it from scratch right now. If object API works fine through cl4py then I consider that a win!

Let's check documentation of py4cl to verify on how python objects are transformed in common lisp.



References:

1. Numba todo