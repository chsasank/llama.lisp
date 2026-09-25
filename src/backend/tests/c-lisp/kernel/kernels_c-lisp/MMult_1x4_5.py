def init_c(c, ldc):
    return [
        ("store", ("ptradd", c, ("add", 0, ("mul", ele, ldc))), 0.0) for ele in range(3)
    ]


def elemental_muladd_1x4(a, b, c, lda, ldb, ldc, loop_var):
    elemental_muladd = [
        (
            "store",
            ("ptradd", c, ("mul", ele, ldc)),
            (
                "fadd",
                ("load", ("ptradd", c, ("mul", ele, ldc))),
                (
                    "fmul",
                    (
                        "load",
                        (
                            "ptradd",
                            a,
                            (
                                "mul",
                                loop_var,
                                lda,
                            ),
                        ),
                    ),
                    ("load", ("ptradd", b, ("add", loop_var, ("mul", ele, ldb)))),
                ),
            ),
        )
        for ele in range(4)
    ]
    return elemental_muladd
