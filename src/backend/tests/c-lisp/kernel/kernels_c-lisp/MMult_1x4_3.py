def elemental_muladd_1x4(a, b, c, k, lda, ldb, ldc):
    elemental_add = [
        ('call', '__add_dot',
            k,
            a,
            lda,
            ('ptradd', b, ('mul', ele, ldb)),
            ('ptradd', c, ('mul', ele, ldc))) for ele in range(4)
    ]
    return elemental_add