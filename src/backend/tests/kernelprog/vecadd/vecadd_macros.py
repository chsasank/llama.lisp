from numba_drvapi import API_PROTOTYPES
from ctypes import (
    c_byte,
    c_char_p,
    # c_float,
    c_int,
    # c_size_t,
    c_uint,
    # c_uint8,
    c_void_p,
    # py_object,
    # CFUNCTYPE,
    c_ulong,
    POINTER,
)

def get_eof():
    stdio_h = open("/usr/include/stdio.h")
    eof_char = None # Set the correct value here if the script cannot determine it
    for line in stdio_h:
        if line.startswith("#define EOF"):
            eof_char = eval(line.split()[2])

    if eof_char is None:
        raise Exception("Could not determine the EOF character for this platform. Please set it manually")
    return eof_char

def get_clisp_type(c_type):
    ptr_int8 = ["ptr", "int8"]
    type_map = {
        ## Ctypes type -> C-Lisp type
        # c_byte: "int8",
        # c_int: "int32",
        # c_uint: "int32",
        # c_void_p: ptr_int8,
        # c_char_p: ptr_int8,
        # c_ulong:
        "b": "int8",
        "i": "int", "I": "int",
        "P": ptr_int8, # Opaque pointer
        "z": ptr_int8, # Character pointer
        "L": "int64",
    }
    if 'contents' in dir(c_type):
        # This is a pointer type
        return ["ptr", get_clisp_type(c_type._type_)]
    elif c_type._type_ in type_map:
        # This is one of the fundamental types
        return type_map[c_type._type_]
    else: raise Exception(f"Unknown type {c_type}")

def get_cuda_signatures(*funcs):
    signatures = []
    for func in funcs:
        ctypes_types = API_PROTOTYPES[func] # (ret_type, arg1_type, arg2_type, ...)
        clisp_types = [get_clisp_type(typ) for typ in ctypes_types]
        parm_list = [(chr(ord('a') + t), typ) for t, typ in enumerate(clisp_types[1:])]
        ret_type = clisp_types[0]
        signatures.append(["define", [[func, ret_type], *parm_list]])

    return signatures

EOF = ["trunc", get_eof(), "int8"]

void_ptr_to = lambda obj: ["bitcast", ["ptr-to", obj], ["ptr", "int8"]]

if __name__ == "__main__":
    print(f"Determined EOF value: {EOF}")
