;; Bitwise shift operations: shl, lshr, ashr.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (shl 1 4))
        (call print (lshr 255 3))
        (call print (ashr -32 3))
        (call print (shl 0 5))
        (ret)))