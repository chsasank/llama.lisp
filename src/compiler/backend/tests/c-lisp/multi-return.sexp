;; Multiple early return statements.
(c-lisp
    (define ((print int) (n int)))

    (define ((sign int) (x int))
        (if (lt x 0)
            (ret -1))
        (if (gt x 0)
            (ret 1))
        (ret 0))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (call sign -7))
        (call print (call sign 0))
        (call print (call sign 7))
        (ret)))
