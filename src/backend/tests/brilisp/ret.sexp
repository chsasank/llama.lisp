(brilisp
    (define ((print int) (n int)))

    (define ((main int))
        (set (v int) (const 4))
        (set (tmp int) (call print v))
        (ret tmp)
        (set (tmp2 int) (call print v))
        (ret tmp2)))