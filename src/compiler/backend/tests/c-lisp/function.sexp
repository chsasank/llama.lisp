(c-lisp
    (define ((print int) (n int)))

    (define ((func int) (b bool) (n int))
        (if b
            ((call print n))
            ((call print 0)))
        (ret n))

    (define ((main void))
        (call func #f 3)
        (call func #t 4)
        (ret)))
