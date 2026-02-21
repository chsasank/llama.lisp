(brilisp
    (define ((print int) (n int)))

    (define ((main int))
        ;testing fptosi on a floating point number
        (set (five int) (const 5))
        (set (a float) (const -9.7123))
        (set (f_n int) (fptosi a int))
        (set (sum int) (add f_n five))
        (set (tmp int) (call print sum))

        ;testing fptosi on a integer number
        (set (c int) (const 9))
        (set (f_n int) (fptosi c int))
        (set (sum int) (add f_n five))
        (set (tmp int) (call print sum))

        (ret tmp)))
