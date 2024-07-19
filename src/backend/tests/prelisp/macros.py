cudevice = "int"
cuptr = ["ptr", "int"]

def cudevice2():
    return "float"


def myError(expr):
    expr[-1] = 3
    return ("call", "error", expr)


def add_print_includes():
    return [
        ("define", (("print", "int"), ("n", "int"))),
        ("define", (("fprint", "float"), ("n", "float"))),
    ]
