(brilisp
    (bril-define ((print int) (n int)))

    (bril-define ((print4 void))
        (set (v int) (const 4))
        (set (tmp int) (call print v))
        (ret))

    (bril-define ((main int))
        (set (v int) (const 2))
        (set (tmp void) (call print4))
        (set (tmp int) (call print v))
        (ret tmp)))
