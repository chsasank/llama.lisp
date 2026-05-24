def row_muladd(k, a, b, c, lda, ldb, ldc, row_num):
    row = [
        (
            "call",
            "__add_dot",
            k,
            ("ptradd", a, row_num),
            "lda",
            ("ptradd", b, ("mul", ele, ldb)),
            ("ptradd", c, ("add", row_num, ("mul", ele, ldc))),
        )
        for ele in range(4)
    ]

    return row
