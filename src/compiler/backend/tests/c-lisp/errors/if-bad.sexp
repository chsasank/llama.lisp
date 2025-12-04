(c-lisp
    (define ((print int) (n int)))

    (define ((main void))
        (if #t
            (call print 5)
            (call print 4)
            (call print 3))))
