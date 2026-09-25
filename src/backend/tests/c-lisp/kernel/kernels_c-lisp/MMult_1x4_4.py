def init_c(c, ldc):
    return [
        ("store", ("ptradd", c, ("add", 0, ("mul", ele, ldc))), 0.0) for ele in range(3)
    ]


def elemental_muladd_1x4(k, a, b, c, lda, ldb, ldc):
    elemental_muladd = [
        (
            "for",
            (("set", "p", 0), ("lt", "p", k), ("set", "p", ("add", "p", 1))),
            (
                "store",
                ("ptradd", c, ("add", 0, ("mul", ele, ldc))),
                (
                    "fadd",
                    ("load", ("ptradd", c, ("add", 0, ("mul", ele, ldc)))),
                    (
                        "fmul",
                        ("load", ("ptradd", a, ("add", 0, ("mul", "p", lda)))),
                        ("load", ("ptradd", b, ("add", "p", ("mul", ele, ldb)))),
                    ),
                ),
            ),
        )
        for ele in range(4)
    ]

    return elemental_muladd
