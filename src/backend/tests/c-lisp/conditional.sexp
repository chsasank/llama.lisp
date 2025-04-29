(c-lisp
    (declare ((print int) (n int)))

    (define ((main int))
        (if #t
            ((call print 1)
             (call print 2)))

        (if #f
            ((call print 0))
            ((call print 3)))
             (ret 0)))
