# C bindings generation for C-Lisp

This is an effort to automate the process of generating bindings to libraries
using their C header files. Parsing and resolving types is offloaded to
Clang/LLVM, while [`gen_bindings.py`](./gen_bindings.py) maps LLVM types to C-Lisp
types and exposes the binding generation machinery via C-Lisp macros.

At the moment, support exists for inclusion of functions that use built-in
C types. Of course, types that can't be expressed as C-Lisp types cannot be used.

## How it works
The process is as follows:
* A C program that uses the desired functions is generated
* The C program above is compiled to LLVM IR using CLang
* The resulting LLVM IR is parsed using [`LLVMLite's binding layer`](https://llvmlite.readthedocs.io/en/latest/user-guide/binding/index.html)

Such a process saves the complication of resolving types that use the higher-level
features of C that don't exist in C-Lisp, such as `typedef`, `enum`, etc.

## Usage in a C-Lisp program
`gen_bindings.py` provides the `include` macro for function inclusion. `include` takes
2 arguments: the list of headers to include in the intermediate C program, and the list
of functions for which to generate declarations.

For example:

```
,@(include
    (stdio.h stdlib.h string.h) ; Headers
    (malloc puts strcpy)) ; Functions to include
```

is replaced by the declarations for `malloc`, `puts` and `strcpy`.


## Running `gen_bindings.py` interactively
`gen_bindings.py` can be run interactively for testing and debugging, in which case it will ask
the user for desired headers and functions, and dump the generated C program to the console.

```
$ python ./gen_bindings
```

```
Space-delimited list of headers to include: stdlib.h
Space-delimited list functions to parse: malloc
#include "stdlib.h"
extern void pin_function(void *);
int main () {
    pin_function(malloc);
}
[['define', [['malloc', ['ptr', 'int8']], ['arg-0', 'int64']]]]

```

See also: [Extracting C Bindings using Clang](https://outline.von-neumann.ai/s/030e5531-946b-4caa-9d5c-3dd78e76756c)
