(c-lisp
    (declare ((print int) (n int)))

    (define ((fib int) (n int))
        (declare last-2 int)
        (declare last-1 int)
        (declare res int)
        (declare i int)

        (set last-2 0)
        (set last-1 1)

        (for ((set i 3) (le i n) (set i (add i 1)))
            (set res (add last-1 last-2))
            (set last-2 last-1)
            (set last-1 res))

        (ret res))

    (define ((main void))
        (call print (call fib 10))
        (ret)))
