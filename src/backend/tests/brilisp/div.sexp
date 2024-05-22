(brilisp
    (bril-define ((print int) (n int)))

    (bril-define ((main int))
        (set (v0 int) (const 9))
        (set (v1 int) (const -20))
        (set (res int) (div v0 v1))
        (set (tmp int) (call print res))
        (ret tmp)))
