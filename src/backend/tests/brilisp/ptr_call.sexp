(brilisp
    (bril-define ((funcA void) (p (ptr int)))
        (ret))

    (bril-define ((main void))
        (set (five int) (const 5))
        (set (x (ptr int)) (alloc five))
        (set (tmp void) (call funcA x))
        (ret)))
