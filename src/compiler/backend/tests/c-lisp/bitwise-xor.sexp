;; Bitwise XOR operation.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (xor 0 0))
        (call print (xor 255 0))
        (call print (xor 85 170))
        (call print (xor 123 123))
        (call print (xor -1 5))
        (ret)))