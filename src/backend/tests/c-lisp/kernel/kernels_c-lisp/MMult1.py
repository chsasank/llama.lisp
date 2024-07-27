def accumlate(a, b, i, j, p, m, k, acc):
    return  ('for', (('set', p, 0), ('lt', p, k), ('set', p, ('add', p, 1))),
                    ('set', acc, ('fadd',
                        ('fmul', 
                            ('load', ('ptradd', a, ('add', ('mul', p, m), i))),
                            ('load', ('ptradd', b, ('add', ('mul', j, k), p)))),
                        acc)))