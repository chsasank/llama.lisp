(brilisp
    (bril-define ((print int) (n int)))

    (bril-define ((main void))
        (set (s int) (const 5))
        (set (p1 (ptr int)) (alloc s))
        (set (three int) (const 3))
        (store p1 three)
        (set (val int) (load p1))
        (set (tmp int) (call print val))
        (ret)))
