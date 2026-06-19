;; Nested if conditionals.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare x int)
        (set x 7)

        (if (lt x 10)
            (if (gt x 5)
                (call print 1)   ; 5 < x < 10
                (call print 2))  ; x <= 5
            (call print 3))       ; x >= 10

        (if (eq x 7)
            ((if (ne x 0)
                (call print 4)
                (call print 0))))

        (ret)))
