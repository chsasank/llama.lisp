;; Unsigned vs signed division on negative operands.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare n int)
        (set n -7)
        (call print (div n 3))
        (call print (udiv n 3))
        (call print (rem n 3))
        (call print (urem n 3))
        (ret)))