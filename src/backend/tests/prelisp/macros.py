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


def init_zeros(c, ldc):
    out = []
    for i in range(4):
        for j in range(4):
            ln = ("store", ("ptradd", c, ("add", i, ("mul", j, ldc))), 0.0)
            out.append(ln)
    return out


def store_vals(p, a, b, c, lda, ldb, ldc):
    out = []
    for i in range(4):
        for j in range(4):
            c_ij = ("ptradd", "c", ("add", i, ("mul", j, "ldc")))
            a_ip = ("ptradd", "a", ("add", i, ("mul", p, "lda")))
            b_pj = ("ptradd", "b", ("add", p, ("mul", j, "ldb")))
            ln = (
                "store",
                c_ij,
                ("fadd", ("load", c_ij), ("fmul", ("load", a_ip), ("load", b_pj))),
            )
            out.append(ln)

    return out


def arr_init(arr, values, length):
    return [
        ["set", arr, ["alloc", "int", length]],
        *(["store", ["ptradd", arr, v], val] for v, val in enumerate(values)),
    ]


def arr_idx(arr, ld_size, i, j):
    "Assuming column major"
    return ("ptradd", arr, ("add", i, ("mul", j, ld_size)))


def muladd(c, a, b):
    return ("store", c, ("fadd", ("load", c), ("fmul", ("load", a), ("load", b))))
