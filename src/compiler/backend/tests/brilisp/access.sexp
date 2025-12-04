(brilisp
    (define ((print int) (n int)))

    (define ((main int))
        (set (inc int) (const 1))
        (set (v int) (const 4545454))
        (set (max int) (const 8989898))
        (set (p (ptr int)) (alloc v))
        (set (count int) (const 0))

        (label lbl)
        (set (count int) (add count inc))
        (store p v)
        (set (val int) (load p))
        (set (loop bool) (ge count max))
        (br loop end lbl)

        (label end)
        (set (tmp int) (call print count))
        (ret tmp)))
