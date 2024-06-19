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
        (declare (arr1 (ptr int)))
        (declare (arr2 (ptr int)))
        (declare (i int))
        (declare (arr_i (ptr int)))

        (set arr1 (alloc int 10))
        (set arr2 (alloc int 10))

        (for ((set i 0)
              (lt i 10)
              (set i (add i 1)))
            (set arr_i (ptradd arr1 i))
            (store arr_i i)
            (set arr_i (ptradd arr2 i))
            (store arr_i (mul i i)))

        (call print (call arr_sum arr1 10))
        (call print (call arr_sum arr2 10))
        (ret)))
