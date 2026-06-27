;; Bitwise AND operation.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (and 12 10))
        (call print (and 255 0))
        (call print (and 85 170))
        (call print (and 123 123))
        (ret)))
