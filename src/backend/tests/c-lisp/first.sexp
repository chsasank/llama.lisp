(c-lisp
    (declare ((print int) (n int)))

    (define ((main int))
        (declare res int)
        (set res (call print 5))
        (ret 0)))
