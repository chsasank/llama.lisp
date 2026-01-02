(brilisp
    (define ((print int) (n int)))
    (define-global (a int) (const 20))
    (define ((main void))
        (set (b int) (const 10))
        (set (tmp int) (add a b))
        (set (res int) (call print tmp))

        ; modify a and check if it still works
        (set (c int) (const 5))
        (set (a int) (sub a c))
        (set (tmp2 int) (add a b))
        (set (res2 int) (call print tmp2))
        (ret)))
