(c-lisp
    (define ((print int) (n int)))

    (define ((cons void) (arr (ptr int)) (n int))
        (declare i int)
        (for ((set i 0)
              (lt i n)
              (set i (add i 1)))
            (store
                (ptradd arr i)
                (mul i 3))))

    (define ((find int) (arr (ptr int)) (key int) (n int))
        (declare i int)
        (for ((set i 0)
                (lt i n)
                (set i (add i 1)))
            (if (eq key
                    (load (ptradd arr i)))
                (ret i))))

    (define ((verify int) (arr (ptr int)) (key int) (idx int) (len int))
        (declare val int)
        (if (lt idx len)
            ((set val (load (ptradd arr idx)))
             (if (eq val key)
                 (ret val)
                 (ret -1)))
            (ret -2)))

    (define ((main void))
        (declare input (ptr int))
        (declare i int)

        (set input (alloc int 10))
        (call cons input 10)

        (declare 12-idx int)
        (declare 13-idx int)

        (set 12-idx (call find input 12 10))
        (set 13-idx (call find input 13 10))

        (call print (call verify input 12 12-idx 10))
        (call print (call verify input 13 13-idx 10))
        (call print (call verify input 12 2 10))
        (ret)))
