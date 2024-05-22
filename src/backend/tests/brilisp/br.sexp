(brilisp
    (bril-define ((print int) (n int)))

    (bril-define ((main int))
        (set (v int) (const 4))
        (set (b bool) (const false))
        (br b there here)
        (label here)
        (set (v int) (const 2))
        (label there)
        (set (tmp int) (call print v))
        (ret tmp)))
