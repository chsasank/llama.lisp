(brilisp
    (bril-define ((print int) (n int)))

    (bril-define ((main int))
        (set (v int) (const 5))
        (set (tmp int) (call print v))
        (ret tmp)))