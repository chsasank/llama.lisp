;; Basic while loop.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare i int)
        (declare sum int)
        (set i 0)
        (set sum 0)
        (while (lt i 10)
            (set sum (add sum i))
            (set i (add i 1)))
        (call print sum)
        (ret)))
