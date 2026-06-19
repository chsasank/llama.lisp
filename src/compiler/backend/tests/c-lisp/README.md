# C-Lisp compiler backend tests

This directory contains CPU-runnable c-lisp tests for the `tile_vendor/llama.lisp` compiler backend. Each `.sexp` file is a c-lisp program; the matching `.out` file is the expected stdout captured by [turnt](https://github.com/cucapra/turnt).

## Running the tests

From this directory:

```bash
# Run all c-lisp tests at the default optimization level (-O2)
turnt *.sexp

# Run a single test
turnt vector_copy.sexp

# Run at a different clang optimization level
OPT_LEVEL=-O0 turnt *.sexp
OPT_LEVEL=-O1 turnt *.sexp
OPT_LEVEL=-O3 turnt *.sexp
```

The parent directory also provides `run_tests.sh`, which runs the full compiler backend suite (brilisp, c-lisp, prelisp, parser, cuda-lisp, and kernelprog).

## Regenerating baselines

If a test output changes intentionally, update the `.out` file with:

```bash
turnt --save vector_copy.sexp
```

## Test categories

### Basic smoke test
- `first.sexp` — Minimal call to an external print function.

### Variables, arithmetic, and operators
- `binary-ops.sexp` — Integer arithmetic and boolean equality.
- `rem.sexp` — Remainder/modulo operator.
- `int-div.sexp` — Integer division.
- `iteration.sexp` — `for` loop edge cases (zero-trip, normal, final value).
- `literals.sexp` — Negative and edge integer/float/boolean literals.

### Boolean logic
- `bool-ops.sexp` — `and`, `or`, `not` on boolean values.

### Control flow
- `conditional.sexp` — `if` / `if-else` with compound branches.
- `nested-if.sexp` — Nested `if` conditionals.
- `for-return.sexp` — Returning from inside a `for` loop; function fall-through handling.
- `while-basic.sexp` — Basic `while` loop.
- `while-compound.sexp` — `while` loop with a compound body and nested scope.
- `while-return.sexp` — Early `ret` from inside a `while` loop.

### Functions
- `function.sexp` — Basic function definition and call with arguments.
- `inline-function.sexp` — `define-inline` with `alwaysinline` attribute.
- `brilisp-function.sexp` — `define-brilisp` function independent of asm/intrinsics.
- `multi-return.sexp` — Multiple early `(ret ...)` statements.
- `ptr-return.sexp` — Function returning a pointer.
- `many-args.sexp` — Function with several arguments.
- `mutual-recursion.sexp` — Mutually recursive functions via forward declarations.
- `fibonacci-recursive.sexp` — Recursive function.
- `fibonacci-dynamic-prog.sexp` — Dynamic-programming style function.
- `prime.sexp` — Function with nested loops.
- `bisection-rootfind.sexp` — Numeric algorithm using functions.
- `sum-square-difference.sexp` — Multi-function arithmetic algorithm.
- `gaussian-elimination.sexp` — Larger algorithm combining arrays and functions.

### Floating-point operations
- `float-arith.sexp` — Native `fadd`, `fsub`, `fmul`, `fdiv`.
- `float-compare.sexp` — Native `feq`, `fne`, `flt`, `fgt`, `fle`, `fge`.

### Arrays
- `array-sum.sexp` — Heap-allocated arrays via `(ptr int)`.
- `array-sum-arr.sexp` — Stack-allocated arrays via `(arr N int)` and `aptradd`.
- `matrix-sum.sexp` — 2D array indexing.
- `insertion-sort.sexp` — In-place array sorting.
- `transpose.sexp` — Nested loops and in-place matrix transpose.
- `alloc-float.sexp` — Heap-allocated `float` and `double` arrays.
- `struct-array.sexp` — Array whose element type is a struct.

### Pointers and references
- `local-ptr.sexp` — Heap pointer allocation, `ptradd`, `store`, `load`.
- `ptr-to-unassigned.sexp` — `ptr-to` on uninitialized variables.
- `ptr-to-function.sexp` — `ptr-to` inside a function.
- `alloc-ptr.sexp` — Array of pointers.
- `list.sexp` — Linked list using struct pointers.

### Structs
- `struct.sexp` — Basic struct definition, `sptradd`, `ptr-to`.
- `struct-var.sexp` — Struct passed by value, stored and loaded.
- `nested-struct.sexp` — Struct field whose type is another struct.
- `struct-array.sexp` — Array whose element type is a struct.
- `struct-return.sexp` — C-Lisp function returning a struct by value.
- `struct-assign.sexp` — Copying a struct value from one variable to another.
- `struct-asm-extractvalue.sexp` — Struct returned from inline asm plus `extractvalue`.

### Strings
- `string.sexp` — String literals, `strcmp`, `puts`.
- `null-term-string.sexp` — Manual null-terminated char array.

### Type casts and conversions
- `casts.sexp` — `bitcast`, `fpext`/`fptrunc`, `sext`/`zext`/`trunc`, `sitofp`/`uitofp`, `fptosi`/`fptoui`, `ptrtoint`/`inttoptr`.
- `bitcast-float-int.sexp` — `bitcast` between `float` and `int`.

### Scope
- `scope.sexp` — Variable shadowing inside nested blocks and loops.

### Globals and address spaces
- `global.sexp` — Global scalar variables.
- `global-arr.sexp` — Global arrays.
- `global-arr-init.sexp` — Global array with a `const` initializer.
- `global-arr-addrspace.sexp` — Global array in addrspace 3.
- `global-ptr.sexp`, `global-ptr-2.sexp` — Global pointers.
- `global-ptr-init.sexp` — Global pointer initialized with `ptr-to` another global.
- `addrspacecast.sexp` — `addrspacecast` between address spaces.
- `addrspace_shared_mem.sexp` — CUDA shared-memory pattern (`addrspace 3`, NVVM intrinsics).

### Inline assembly
- `asm-one-output.sexp` — Single `=r` output.
- `asm-zero-outputs.sexp` — Side-effect asm with a pointer input.
- `asm-multi-output.sexp` — Multi-output asm returning a struct.
- `asm-mixed-registers.sexp` — Mixed `=r` (int) and `=x` (SSE float) outputs.
- `asm-struct-as-arg.sexp` — Struct from one asm fed into another asm via `extractvalue`.
- `asm-intrinsic-add.sexp` — `define-inline` wrapper around an asm add.
- `asm-intrinsic-pair.sexp` — `define-inline` returning a struct from asm.
- `asm-intrinsic-mem.sexp` — `define-inline` wrappers for asm load/store.

### Memory / LLVM intrinsics
- `vector_copy.sexp` — `llvm.memcpy` intrinsic wrapped in `define-inline`.

## Adding a new test

1. Create a new `<name>.sexp` file in this directory.
2. Run `turnt --save <name>.sexp` to generate the baseline `<name>.out`.
3. Verify the test passes at multiple optimization levels:
   ```bash
   for opt in O0 O1 O2 O3; do OPT_LEVEL=-$opt turnt <name>.sexp; done
   ```
4. Add the test to the category list above.
