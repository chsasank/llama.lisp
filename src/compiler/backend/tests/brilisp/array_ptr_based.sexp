(brilisp
    (define ((print int) (n int)))

    (define ((main void))
        (set (b int) (const 5))
        (set (len int) (const 10))
        (set (a (ptr int)) (alloc len))

        (set (indx int) (const 3))
        (set (arr_i (ptr int)) (ptradd a indx))
        (set (val int) (const 6))
        (store arr_i val)

        (set (v int) (load arr_i))
        (set (result int) (add b v))
        (set (tmp int) (call print result))
        (ret)))
