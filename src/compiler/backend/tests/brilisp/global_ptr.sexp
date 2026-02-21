(brilisp
    (define ((print int) (n int)))
    (define-global (a (ptr int)))

    (define ((main void))
        (set (n int) (const 20))
        (set (b (ptr int)) (alloc n))
        (set (c (ptr int)) (alloc n))
        ; because global variables are pointers
        (store a c)

        (set (four int) (const 4))
        (set (a_load (ptr int)) (load a))
        (set (a_i (ptr int)) (ptradd a_load 3))
        (set (b_i (ptr int)) (ptradd b 3))

        (store a_i four)
        (store b_i four)

        (set (a_load (ptr int)) (load a))
        (set (a_i_1 (ptr int)) (ptradd a_load 3))
        (set (b_i_1 (ptr int)) (ptradd b 3))

        (set (a_i_load int) (load a_i_1))
        (set (b_i_load int) (load b_i_1))

        (set (tmp int) (call print a_i_load))
        (set (tmp int) (call print b_i_load))



        (ret)))
