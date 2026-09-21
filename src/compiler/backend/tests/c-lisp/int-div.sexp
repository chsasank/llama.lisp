;; Integer division.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (div 17 5))
        (call print (div -17 5))
        (ret)))
