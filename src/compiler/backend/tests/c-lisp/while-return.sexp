;; Early return from inside a while loop.
(c-lisp
    (define ((print int) (n int)))

    (define ((first-divisor int) (n int))
        (declare i int)
        (set i 2)
        (while (le (mul i i) n)
            (if (eq (rem n i) 0)
                (ret i))
            (set i (add i 1)))
        (ret -1))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (call first-divisor 35))
        (call print (call first-divisor 37))
        (ret)))
