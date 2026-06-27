;; Bitwise OR operation.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (or 0 0))
        (call print (or 255 0))
        (call print (or 85 170))
        (call print (or 123 123))
        (ret)))
