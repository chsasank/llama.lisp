#!/bin/python3

import re
import sys
import subprocess
import tempfile
from llvmlite import binding

## gen_bindings.py
## Provides macros to include forward
## declarations from C header files


def gen_cprog(headers, functions):
    """Generate the C program for binding generation"""

    # C code to produce an external side-effect involving each function
    function_pins = "\n".join(f"    pin_function({func});" for func in functions)
    # Header inclusion
    header_includes = "\n".join(f'#include "{header}"' for header in headers)

    return "\n".join(
        (
            header_includes,
            "extern void pin_function(void *);",
            "int main () {",
            function_pins,
            "}",
        )
    )


def get_clisp_type(typ):
    """Map an LLVM type (str) to a C-Lisp type"""

    ## Handlers for each type kind
    def get_clisp_int_type(typ):
        """'iXX' -> 'intXX'"""
        width = typ[1:]
        if width == "32":
            width = ""
        return f"int{width}"

    def get_clisp_ptr_type(typ):
        """'XX*' -> ['ptr', 'XX']"""
        pointee = typ[:-1]
        return ["ptr", get_clisp_type(pointee)]

    type_map = (
        ## Type regex -> handler function
        (r"i[0-9]+", get_clisp_int_type),
        (r".*\*", get_clisp_ptr_type),
        ("void", lambda t: "void"),
    )
    for pattern, handler in type_map:
        if re.fullmatch(pattern, typ):
            return handler(typ)
    raise Exception(f"Unknown type: {typ}")


def gen_signature(fn_ref):
    """Generate the signature of fn_ref (LLVMLite ValueRef)"""
    # fn_ref is a function pointer; fn_type is the function's type
    fn_type = fn_ref.type.element_type
    type_elements = [get_clisp_type(str(elem)) for elem in fn_type.elements]
    signature = [
        "define",
        [
            [fn_ref.name, type_elements[0]],
            *([f"arg-{t}", typ] for t, typ in enumerate(type_elements[1:])),
        ],
    ]
    return signature


def include(headers, functions, debug=False):
    """
    Macro to include given functions from given header files
    Example usage:
        ; Generates declarations for `malloc` and `puts`, having included stdio.h and stdlib.h
        ,(include (stdio.h stdlib.h) (malloc puts))
    """

    tmpdir = tempfile.TemporaryDirectory()
    cprog = f"{tmpdir.name}/binding.c"
    llprog = f"{tmpdir.name}/binding.ll"

    # Generate C program using desired functions
    cprog_file = open(cprog, "w")
    cprog_str = gen_cprog(headers, functions)
    if debug:
        print(cprog_str)
    cprog_file.write(cprog_str)
    cprog_file.close()

    # Compile to LLVM
    subprocess.run(f"clang -o {llprog} -emit-llvm -S {cprog}".split())

    # Parse the LLVM IR and get a handle to the module
    llprog_file = open(llprog)
    llprog_str = llprog_file.read()
    llmod = binding.parse_assembly(llprog_str)
    llprog_file.close()

    # Cleanup
    tmpdir.cleanup()

    # Parse signatures of desired functions
    return [gen_signature(llmod.get_function(func)) for func in functions]


if __name__ == "__main__":
    headers = input("Space-delimited list of headers to include: ").split()
    functions = input("Space-delimited list functions to parse: ").split()
    print(include(headers, functions, debug=True))
