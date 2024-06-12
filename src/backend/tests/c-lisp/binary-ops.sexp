(c-lisp
    (define ((print int) (n int)))

    (define ((main int))
        (declare (five int))
        (declare (twenty int))
        (declare (res bool))

        (set five 5)
        (set twenty 20)
        (set res (==
                    (* five 6)
                    (+ 10 twenty)))

        (if res
            (call print 0))
        (ret 0)))
