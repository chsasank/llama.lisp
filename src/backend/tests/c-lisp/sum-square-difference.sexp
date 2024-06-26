(c-lisp
    (define ((print int) (n int)))

    (define ((sum-square-diff int) (n int))
        (declare sum-sq int)
        (set sum-sq 0)
        (declare sum int)
        (set sum 0)

        (declare i int)
        (for ((set i 1) (le i n) (set i (add i 1)))
            (set sum-sq (add sum-sq (mul i i)))
            (set sum (add sum i)))

        (ret (sub (mul sum sum) sum-sq)))

    (define ((main void))
        (call print (call sum-square-diff 10))
        (ret)))
