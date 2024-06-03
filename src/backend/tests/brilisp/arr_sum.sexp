(brilisp
    (bril-define ((print int) (n int)))

    (bril-define ((main int))
        (set (len int) (const 10))
        (set (arr (ptr int)) (alloc len))
        (set (sum int) (const 0))
        (set (idx int) (const 0))
        (set (one int) (const 1))

        (label init)
        (set (arr_i (ptr int)) (ptradd arr idx))
        (set (idx int) (add idx one))
        (store arr_i idx)
        (set (loop bool) (lt idx len))
        (br loop init calc)

        (label calc)
        (set (idx int) (sub idx one))
        (set (arr_i (ptr int)) (ptradd arr idx))
        (set (v int) (load arr_i))
        (set (sum int) (add sum v))
        (set (loop bool) (ge idx one))
        (br loop calc out)

        (label out)
        (set (tmp int) (call print sum))
        (ret tmp)))
