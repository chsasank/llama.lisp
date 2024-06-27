(c-lisp
    (define ((print int) (n int)))

    (define ((cons void) (arr (ptr int)) (n int))
        (declare (i int))
        (for ((set i 0)
              (lt i n)
              (set i (add i 1)))
            (store
                (ptradd arr i)
                (mul i 3))))

    (define ((find int) (arr (ptr int)) (key int) (n int))
        (declare (i int))
        (for ((set i 0)
                (lt i n)
                (set i (add i 1)))
            (if (eq key
                    (load (ptradd arr i)))
                (ret i))))

    (define ((main void))
        (declare (input (ptr int)))
        (declare (i int))

        (set input (alloc int 10))
        (call cons input 10)
        (call print (call find input 12 10))
        (ret)))
