(brilisp
    (bril-define ((print int) (n int)))

    (bril-define ((add5 int) (n int))
        (set (five int) (const 5))
        (set (sum int) (add n five))
        (ret sum))

    (bril-define ((main int))
        (set (a int) (const 9))
        (set (b int) (call add5 a))
        (set (tmp int) (call print b))
        (ret b)))