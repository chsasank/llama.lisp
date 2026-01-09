(brilisp
    (define ((print int) (n int)))
    (define-global (a int) (const 20))
    (define ((main void))
        (set (b int) (const 10))
        (set (a_load int) (load a))
        (set (tmp int) (add a_load b))
        (set (res int) (call print tmp))

        ; modify a and check if it still works
        (set (c int) (const 5))

        (set (a_store int) (sub a_load c))
        (store a a_store)
        (set (a_load int) (load a))
        (set (tmp2 int) (add a_load b))
        (set (res2 int) (call print tmp2))
        (ret)))
