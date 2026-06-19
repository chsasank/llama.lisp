;; While loop with a compound body and nested scope.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare i int)
        (declare sum int)
        (declare prod int)
        (set i 1)
        (set sum 0)
        (set prod 1)
        (while (le i 4)
            ((set sum (add sum i))
             (set prod (mul prod i))
             (set i (add i 1))))
        (call print sum)
        (call print prod)
        (ret)))
