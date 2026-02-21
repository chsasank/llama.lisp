(brilisp
    (define ((print int) (n int)))

    (define ((main int))
        (set (inc int) (const 1))
        (set (v int) (const 4545))
        (set (max int) (const 8989898))
        (set (p (ptr int)) (alloc v))
        (set (arr (ptr (ptr int))) (alloc v))
        (set (count int) (const 0))

        (label lbl)
        (set (arr_i (ptr (ptr int))) (ptradd arr count))
        (set (count int) (add count inc))
        (store arr p)
        (set (val (ptr int)) (load arr_i))
        (set (loop bool) (ge count max))
        (br loop end lbl)

        (label end)
        (set (tmp int) (call print count))
        (ret tmp)))
