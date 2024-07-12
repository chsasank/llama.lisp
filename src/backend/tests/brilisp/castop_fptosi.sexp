(brilisp
    (define ((print int) (n int)))

    ;Function to receive a float value
    (define ((add5_float int) (n float))
        (set (five int) (const 5))
        (set (f_n int) (fptosi n int))
        (set (sum int) (add f_n five))
        (ret sum))

    ;Function to receive an int value
    (define ((add5_int int) (n int))
        (set (five int) (const 5))
        (set (f_n int) (fptosi n int))
        (set (sum int) (add f_n five))
        (ret sum))

    (define ((main int))
        ;testing fptosi on a floating point number
        (set (a float) (const -9.7123))
        (set (b int) (call add5_float a))
        (set (tmp int) (call print b))

        ;testing fptosi on a integer number
        (set (c int) (const 9))
        (set (d int) (call add5_int c))
        (set (tmp int) (call print d))
        (ret b)))

