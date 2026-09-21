;; Negative and edge literals.
(c-lisp
    (define ((print int) (n int)))
    (define ((fprint float) (n float)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print -5)
        (call print 0)
        (call print 2147483647)
        (call fprint -3.5)
        (call fprint 0.0)
        (if #t (call print 1) (call print 0))
        (if #f (call print 0) (call print 2))
        (ret)))
