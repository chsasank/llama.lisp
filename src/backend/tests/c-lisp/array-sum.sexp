(c-lisp
    (define ((print int) (n int)))

    (define ((arr_sum int) (a (ptr int)) (n int))
        (declare (i int))
        (declare (sum int))
        (set sum 0)

        (for ((set i 0)
              (lt i n)
              (set i (add i 1)))
            (set sum (add sum (load (ptradd a i)))))
        (ret sum))

    (define ((main void))
        (declare (arr (ptr int)))
        (declare (i int))
        (declare (arr_i (ptr int)))

        (set arr (alloc int 10))

        (for ((set i 0)
              (lt i 10)
              (set i (add i 1)))
            (set arr_i (ptradd arr i))
            (store arr_i i))

        (call print (call arr_sum arr 10))
        (ret)))
