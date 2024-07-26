import numba_drvapi
import sys


def get_eof():
    """ EOF is platform-dependent """
    stdio_h = open("/usr/include/stdio.h")
    eof_char = None # NOTE: Set the correct value here if the script cannot determine it
    for line in stdio_h:
        if line.startswith("#define EOF"):
            eof_char = eval(line.split()[2])

    if eof_char is None:
        raise Exception("Could not determine the EOF character for this platform. Please set it manually")
    return eof_char


def get_cuda_signatures():
    """ Grab CUDA typedefs and signatures from Numba's CUDA driver """
    signatures = []
    for func in numba_drvapi.API_PROTOTYPES:
        ctypes_types = numba_drvapi.API_PROTOTYPES[func] # (ret_type, arg1_type, arg2_type, ...)
        clisp_types = [get_clisp_type(typ) for typ in ctypes_types]
        parm_list = [(chr(ord('a') + t), typ) for t, typ in enumerate(clisp_types[1:])]
        ret_type = clisp_types[0]
        signatures.append(["define", [[func, ret_type], *parm_list]])
    globe = globals() # The global scope of this module
    for memb in dir(numba_drvapi):
        # All numba_drvapi.cu_* variables are CUDA data types as named by Numba
        if memb.startswith("cu_"):
            # Dynamically create the macro variable
            globe[memb] = get_clisp_type(getattr(numba_drvapi, memb))

    return signatures

def get_clisp_type(c_type):
    """ Map a ctypes object to a C-Lisp type """
    ptr_int8 = ["ptr", "int8"]
    type_map = {
        ## C type-> C-Lisp type
        ## Keys here are the _type_ attributes of ctypes types
        "b": "int8", "B": "int8",
        "i": "int", "I": "int",
        "f": "float",
        "P": ptr_int8, # Opaque pointer
        "z": ptr_int8, # Character pointer
        "L": "int64",
    }
    dir_type = dir(c_type)
    if 'contents' in dir_type:
        # This is a pointer type
        return ["ptr", get_clisp_type(c_type._type_)]
    elif '_length_' in dir_type:
        # This is an array type
        return ["ptr", get_clisp_type(c_type._type_)]
    elif '_type_' in dir_type and c_type._type_ in type_map:
        # This is one of the fundamental types
        return type_map[c_type._type_]
    else:
        print(f"[WARNING]: Unknown type {c_type}; overriding with (ptr int8)", file=sys.stderr)
        return ["ptr", "int8"]


def define_printf():
    """ This definition of printf is capable of printing one data value embedded in a format string """
    global _printf_defined
    _printf_defined = True
    return ["define", [["printf", "int"], ["fmt", ["ptr", "int8"]], ["data", "int"]]]

def printf(fmt, data=0):
    """ Call `printf` as defined above, inserting a bitcast on the data argument """
    assert _printf_defined, "You must call ,define_printf in order to use ,(printf)"
    return ["call", "printf", fmt, ["bitcast", data, "int"]]


def error_check(expr):
    """ Error-checking macro """
    expr_string = str(expr)
    return ["call", "error_check", expr, ["string", expr_string]]


void_ptr_to = lambda obj: ["bitcast", ["ptr-to", obj], ["ptr", "int8"]]


# Useful constants and type aliases
voidptr = ["ptr", "int8"]
nullptr = ["inttoptr", 0, voidptr]
EOF = ["trunc", get_eof(), "int8"]


if __name__ == "__main__":
    import json
    print(f"Determined EOF value: {EOF}")
    sigs = get_cuda_signatures()
    sigs_f = open("sigs", "w")
    json.dump(sigs, sigs_f)
    print("Wrote signatures to ./sigs")
