(c-lisp
    (declare ((print int) (n int)))

    (define ((main int))
        (declare five int)
        (declare twenty int)
        (declare res bool)

        (set five 5)
        (set twenty 20)
        (set res (eq
                    (mul five 6)
                    (add 10 twenty)))

        (if res
            (call print 0))
        (ret 0)))
