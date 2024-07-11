(brilisp
    (define ((print int) (n int)))

    (define ((add5 int) (n float))
        (set (five int) (const 5))
        (set (f_n int) (fptosi n int))
        (set (sum int) (add f_n five))
        (ret sum))

    (define ((main int))
        (set (a float) (const 9.7123))
        (set (b int) (call add5 a))
        (set (tmp int) (call print b))
        (ret b)))

