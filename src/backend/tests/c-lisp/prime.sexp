(c-lisp
    (declare ((print int) (n int)))
    (declare ((square int) (i int))
        (ret (mul i i)))

    (define ((gcd int) (a int) (b int))
        (declare big int)
        (declare small int)

        (if (gt a b)
            ((set big a)
             (set small b))

             (if (lt a b)
                ((set small a)
                 (set big b))

                (ret a)))

        (ret (call gcd small (sub big small))))

    (define ((is-prime bool) (n int))
        (declare i int)
        (declare iter int)
        (declare not-prime bool)
        (set not-prime #f)

        (for ((set i 2) (le (call square i) n) (set i (add i 1)))
            (set iter i)
            (set not-prime
                 (ne (call gcd i n) 1))
            (if not-prime
                (set i n))) ; TODO: Use break when implemented

        (call print iter)
        (if not-prime
            (ret #f))
        (ret #t))

    (define ((main void) (n int))
        (if (call is-prime 29)
            (call print 1)
            (call print 0))

        (if (call is-prime 49)
            (call print 1)
            (call print 0))

        (ret)))
