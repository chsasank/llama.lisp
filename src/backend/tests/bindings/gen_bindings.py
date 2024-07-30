#!/bin/python3

import re
import sys
import subprocess
import tempfile
from llvmlite import binding

## gen_bindings.py
## Provides macros to include forward
## declarations from C header files


class BindingGenError(Exception):
    pass


class BindingGenerator:
    def __init__(self, headers, functions, structs):
        # List of headers to be included
        self.headers = headers
        # List of desired functions prototypes
        self.functions = functions
        # List of desired structs definitions
        self.structs = structs

        # The LLVM module containing the signatures we want
        self.llmod = None

    def gen_cprog(self):
        """Generate the C program for binding generation"""

        ## C code to produce external side-effects
        # Functions
        function_pins = "\n".join(
            f"    pin_function({func});" for func in self.functions
        )
        # Structs
        struct_pins = "\n".join(
            f"    {{ struct {struct} var; pin_function(&var); }}"
            for struct in self.structs
        )

        ## Header inclusion
        header_includes = "\n".join(f'#include "{header}"' for header in self.headers)

        return "\n".join(
            (
                header_includes,
                "extern void pin_function(void *);",
                "int main () {",
                function_pins,
                struct_pins,
                "}",
            )
        )

    def get_clisp_type(self, typ):
        """Map an LLVM type (TypeRef) to a C-Lisp type"""

        ## Handlers for each type kind
        ## Each takes a string as an argument
        def get_clisp_int_type(typ):
            """'iXX' -> 'intXX'"""
            width = typ[1:]
            if width == "32":
                width = ""
            return f"int{width}"

        def get_clisp_ptr_type(typ):
            """'XX*' -> ['ptr', 'XX']"""
            pointee = typ[:-1]
            return ["ptr", self.get_clisp_type(pointee)]

        def get_clisp_struct_type(typ):
            """'%struct.XX' -> ['struct', 'XX']"""
            return ["struct", typ.replace("%struct.", "")]

        def get_from_llvm_var(var):
            """'%XX' -> <value of LLVM variable %XX>"""
            pass

        type_map = (
            ## Type regex -> handler function
            (r"i[0-9]+", get_clisp_int_type),
            (r".*\*", get_clisp_ptr_type),
            (r"float|void", lambda typ: str(typ)),
            (r"%struct\..*", get_clisp_struct_type),
        )
        typ_str = str(typ)
        for pattern, handler in type_map:
            if re.fullmatch(pattern, typ_str):
                return handler(typ_str)
        raise BindingGenError(f"Unknown type: {typ}")

    def gen_signature(self, fn_ref):
        """Generate the signature of fn_ref (LLVMLite ValueRef)"""
        # fn_ref is a function pointer; fn_type is the function's type
        fn_type = fn_ref.type.element_type
        type_elements = [self.get_clisp_type(elem) for elem in fn_type.elements]
        return [
            "define",
            [
                [fn_ref.name, type_elements[0]],
                *([f"arg-{t}", typ] for t, typ in enumerate(type_elements[1:])),
            ],
        ]

    def gen_struct(self, struct_ref):
        """Generate a struct type corresponding to struct_ref (LLVMLite TypeRef)"""
        field_types = [self.get_clisp_type(elem) for elem in struct_ref.elements]
        assert struct_ref.name.startswith("struct.")
        return [
            "define-struct",
            struct_ref.name.replace("struct.", ""),
            *([f"field-{t}", typ] for t, typ in enumerate(field_types)),
        ]

    def include(self, debug=False):
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
        cprog_str = self.gen_cprog()
        if debug:
            print_prog(cprog_str)
        cprog_file.write(cprog_str)
        cprog_file.close()

        # Compile to LLVM
        subprocess.run(f"clang -I./ -o {llprog} -emit-llvm -S {cprog}".split())

        # Parse the LLVM IR and get a handle to the module
        llprog_file = open(llprog)
        llprog_str = llprog_file.read()
        if debug:
            print_prog(llprog_str)
        self.llmod = binding.parse_assembly(llprog_str)
        llprog_file.close()

        # Cleanup
        tmpdir.cleanup()

        return [
            # Signatures of desired functions
            *(
                self.gen_signature(self.llmod.get_function(func))
                for func in self.functions
            ),
            # Desired struct types
            *(
                self.gen_struct(self.llmod.get_struct_type(f"struct.{struct}"))
                for struct in self.structs
            ),
        ]


## `include` Prelisp macro
def include(headers, functions, structs):
    binding_generator = BindingGenerator(headers, functions, structs)
    return binding_generator.include()


# Utility functions
def print_prog(prog):
    print("====", "    " + prog.replace("\n", "\n    "), "====", sep="\n")


if __name__ == "__main__":
    headers = input("Space-delimited list of headers to include: ").split()
    functions = input("Space-delimited list functions to parse: ").split()
    structs = input("Space-delimited list structs to parse: ").split()
    binding_generator = BindingGenerator(headers, functions, structs)
    print(binding_generator.include(debug=True))
