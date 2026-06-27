;; Bitwise NOT operation.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (not 0))
        (call print (not -1))
        (call print (not 5))
        (ret)))
