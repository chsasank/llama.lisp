cudevice = "int"

def myError(expr):
    return ("call", "error", expr)

def add_print_includes():
    return [
        ("define", (("print", "int"), ("n", "int"))),
        ("define", (("fprint", "float"), ("n", "float")))
    ]