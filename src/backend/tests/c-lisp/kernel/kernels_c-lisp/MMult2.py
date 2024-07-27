def call_add_dot(a, b, c, lda, ldb, ldc, ele, i, j, k):
    return [
        ('call', '__add_dot', 
            k, 
            ('ptradd', a, ('add', i, ('mul', 0, lda))), 
            lda,
            ('ptradd', b, ('add', 0, ('mul', ('add', j, ele), ldb))),
            ('ptradd', c, ('add', i, ('mul', ('add', j, ele), ldc))))
    ]


