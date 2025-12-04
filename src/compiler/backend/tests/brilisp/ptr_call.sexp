(brilisp
    (define ((funcA void) (p (ptr int)))
        (ret))

    (define ((main void))
        (set (five int) (const 5))
        (set (x (ptr int)) (alloc five))
        (set (tmp void) (call funcA x))
        (ret)))
