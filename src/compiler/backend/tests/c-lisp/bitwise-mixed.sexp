;; Combined bitwise operations: shift + xor (simple hash-mix pattern).
(c-lisp
    (define ((print int) (n int)))

    (define ((hash-mix int) (x int))
        (declare t int)
        (set t (xor x (shl x 13)))
        (set t (xor t (lshr t 17)))
        (set t (xor t (shl t 5)))
        (ret t))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (call hash-mix 1))
        (call print (call hash-mix 123456))
        (call print (call hash-mix -1))
        (ret)))