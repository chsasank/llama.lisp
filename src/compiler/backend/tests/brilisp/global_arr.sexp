(brilisp
    (define ((print int) (n int)))
    (define-global (a (arr 20 int)))

    (define ((main void))
        (set (one int) (const 1))
        (set (b (ptr (arr 10 int))) (alloc one))
        (set (a_i (ptr int)) (ptradd a 0 3))
        (set (b_i (ptr int)) (ptradd b 0 3))

        (set (twenty int) (const 20))
        (store a_i twenty)
        (store b_i twenty)

        (set (a_i_1 (ptr int)) (ptradd a 0 3))
        (set (b_i_1 (ptr int)) (ptradd b 0 3))

        (set (a_i_load int) (load a_i_1))
        (set (b_i_load int) (load b_i_1))

        (set (tmp int) (call print a_i_load))
        (set (tmp int) (call print b_i_load))

        (ret)))
