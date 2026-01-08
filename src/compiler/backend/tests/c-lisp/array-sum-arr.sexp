(c-lisp
    (define ((print int) (n int)))

    (define ((main void))
        (declare arr (arr 10 int))

        (declare i int)
        (for ((set i 0)
              (lt i 10)
              (set i (add i 1)))
            (store (aptradd arr i) i))

        (declare sum int)
        (set sum 0)

        (for ((set i 0)
              (lt i 10)
              (set i (add i 1)))
            (set sum (add sum (load (aptradd arr i)))))

        (call print sum)

        (ret)))
