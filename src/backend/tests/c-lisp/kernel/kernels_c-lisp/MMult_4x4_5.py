def init_c(c, ldc, row):
    return [
        ("store", ("ptradd", c, ("add", row, ("mul", ele, ldc))), 0.0)
        for ele in range(4)
    ]


def elemental_muladd_4x4(a, b, c, lda, ldb, ldc, loop_var, row):
    elemental_muladd = [
        (
            "store",
            ("ptradd", c, ("add", row, ("mul", ele, ldc))),
            (
                "fadd",
                ("load", ("ptradd", c, ("add", row, ("mul", ele, ldc)))),
                (
                    "fmul",
                    ("load", ("ptradd", a, ("add", row, ("mul", loop_var, lda)))),
                    ("load", ("ptradd", b, ("add", loop_var, ("mul", ele, ldb)))),
                ),
            ),
        )
        for ele in range(4)
    ]
    return elemental_muladd
