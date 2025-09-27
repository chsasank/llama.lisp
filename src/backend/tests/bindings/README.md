# C bindings generation for C-Lisp

This is an effort to automate the process of generating bindings to libraries
using their C header files. Parsing and resolving types is offloaded to
Clang/LLVM, while [`gen_bindings.py`](./gen_bindings.py) maps LLVM types to C-Lisp
types and exposes the binding generation machinery via C-Lisp macros.

At the moment, support exists for the inclusion of
* Function prototypes
* Struct definitions

## How it works
The process is as follows:
* A C program that uses the desired objects (functions, structs, etc.) is generated
* The C program above is compiled using Clang, to produce the AST in JSON form and the LLVM IR
* The resulting LLVM IR is parsed using [`LLVMLite's binding layer`](https://llvmlite.readthedocs.io/en/latest/user-guide/binding/index.html),
* Using the resulting LLVM IR and the JSON AST, signatures are processed and included. While the LLVM
  IR is sufficient for type information, the AST is used to get higher-level information that does not
  show up in the LLVM IR, such as struct field names.

Such a process saves the complication of resolving types that use the higher-level
features of C that don't exist in C-Lisp, such as `typedef`, `enum`, etc.

## Usage in a C-Lisp program
`gen_bindings.py` provides the `include` macro for function inclusion. `include` takes
2 arguments: the list of headers to include in the intermediate C program, and the list
of functions for which to generate declarations.

For example:

```
,@(include
    (stdio.h stdlib.h string.h time.h) ; Headers
    (malloc puts strcpy) ; Functions to include
    (tm)) ; Structs to include
```

is replaced by the function prototypes for `malloc`, `puts` and `strcpy` and definition for struct type `tm`.


## Running `gen_bindings.py` interactively
`gen_bindings.py` can be run interactively for testing and debugging, in which case it will ask
the user for desired headers and functions, dump the generated C program and LLVM IR to the console,
and finally output the generated signatures.


This is how an example session looks (shortened for brevity):
```
$ python ./gen_bindings
```
```
Space-delimited list of headers to include: time.h stdlib.h
Space-delimited list functions to parse: malloc
Space-delimited list structs to parse: tm
====
    #include "time.h"
    #include "stdlib.h"
    extern void pin_function(void *);
    int main () {
        pin_function(malloc);
        { struct tm var; pin_function(&var); }
    }
====
====
... LLVM target spec ...
    %struct.tm = type { i32, i32, i32, i32, i32, i32, i32, i32, i32, i64, i8* }

    ; Function Attrs: noinline nounwind optnone uwtable
    define dso_local i32 @main() #0 {
      %1 = alloca %struct.tm, align 8
      call void @pin_function(i8* bitcast (i8* (i64)* @malloc to i8*))
      %2 = bitcast %struct.tm* %1 to i8*
      call void @pin_function(i8* %2)
      ret i32 0
    }

    declare dso_local void @pin_function(i8*) #1

    ; Function Attrs: nounwind
    declare dso_local noalias i8* @malloc(i64) #2

... some annotations ...
====
[['define', [['malloc', ['ptr', 'int8']], ['arg-0', 'int64']]], ['define-struct', 'tm', ('tm_sec', 'int'), ('tm_min', 'int'), ('tm_hour', 'int'), ('tm_mday', 'int'), ('tm_mon', 'int'), ('tm_year', 'int'), ('tm_wday', 'int'), ('tm_yday', 'int'), ('tm_isdst', 'int'), ('tm_gmtoff', 'int64'), ('tm_zone', ['ptr', 'int8'])]]
```

See also: [Extracting C Bindings using Clang](https://outline.von-neumann.ai/s/030e5531-946b-4caa-9d5c-3dd78e76756c)
