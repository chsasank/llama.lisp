;; Unsigned integer division and remainder.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (udiv 17 5))
        (call print (urem 17 5))
        (call print (udiv 100 3))
        (call print (urem 100 3))
        (call print (udiv 7 8))
        (call print (urem 7 8))
        (ret)))