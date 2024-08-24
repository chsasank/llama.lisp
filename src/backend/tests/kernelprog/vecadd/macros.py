import gen_bindings


def include(*pargs):
    return gen_bindings.include(*pargs, macro_ns = globals())


def printf(fmt, data=0):
    """Call `printf` as defined above, inserting a bitcast on the data argument"""
    return ["call", "printf", fmt, ["bitcast", data, "int"]]


printf_signature = [
    "define",
    [["printf", "int"], ["fmt", ["ptr", "int8"]], ["data", "int"]],
]


def error_check(expr):
    """Error-checking macro"""
    expr_string = str(expr)
    return ["call", "error_check", expr, ["string", expr_string]]


def void_ptr_to(obj):
    return ["bitcast", ["ptr-to", obj], ["ptr", "int8"]]


def get_eof():
    """EOF is platform-dependent"""
    stdio_h = open("/usr/include/stdio.h")
    eof_char = (
        None  # NOTE: Set the correct value here if the script cannot determine it
    )
    for line in stdio_h:
        if line.startswith("#define EOF"):
            eof_char = eval(line.split()[2])

    if eof_char is None:
        raise Exception(
            "Could not determine the EOF character for this platform. Please set it manually"
        )
    return eof_char


## Useful constants and type aliases
voidptr = ["ptr", "int8"]
nullptr = ["inttoptr", 0, voidptr]
EOF = ["trunc", get_eof(), "int8"]
