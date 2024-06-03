(brilisp
    (bril-define ((print int) (n int)))

    (bril-define ((main int))
        (set (v int) (const 1))
        (set (p (ptr int)) (alloc v))
        (set (v int) (const 7))
        (store p v)

        (set (p2 (ptr int)) (id p))
        (set (v int) (load p))
        (set (tmp int) (call print v))
        (ret tmp)))
