(brilisp
    (bril-define ((print int) (n int)))

    (bril-define ((main int))
        (set (tmp bool) (const false))
        (set (tmp int) (const 2))
        (set (tmp void) (call print tmp))
        (ret tmp)))
