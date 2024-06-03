(brilisp
    (bril-define ((print int) (n int)))

    (bril-define ((main int))
        (set (inc int) (const 1))
        (set (v int) (const 4545454))
        (set (max int) (const 8989898))
        (set (arr (ptr int)) (alloc v))
        (set (count int) (const 0))

        (label lbl)
        (set (arr_i (ptr int)) (ptradd arr count))
        (set (count int) (add count inc))
        (store arr_i v)
        (set (val int) (load arr_i))
        (set (loop bool) (ge count max))
        (br loop end lbl)

        (label end)
        (set (tmp int) (call print count))
        (ret tmp)))
