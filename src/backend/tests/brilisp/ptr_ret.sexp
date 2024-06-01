(brilisp
    (bril-define ((ident (ptr int)) (p (ptr int)))
        (ret p))

    (bril-define ((main void))
        (set (a int) (const 9))
        (set (b (ptr int)) (alloc a))
        (set (c (ptr int)) (call ident b))
        (ret)))
