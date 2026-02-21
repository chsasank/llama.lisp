(brilisp
    (define ((print int) (n int)))

    (define ((print4 void))
        (set (v int) (const 4))
        (set (tmp int) (call print v))
        (ret))

    (define ((main int))
        (set (v int) (const 2))
        (set (tmp void) (call print4))
        (set (tmp int) (call print v))
        (ret tmp)))
