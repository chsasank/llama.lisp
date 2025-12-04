;; ARGS: 8 23 1 56 2 -5 -2 78 3 -7 -19

(c-lisp
    (define ((print int) (n int)))
    (define ((atoi int) (s (ptr int))))

    (define ((swap void) (a (ptr int)) (b (ptr int)))
        (declare tmp int)
        (set tmp (load b))
        (store b (load a))
        (store a tmp)
        (ret))

    (define ((min int) (arr (ptr int)) (n int))
        (declare i int)
        (declare idx int)

        (set idx 0)
        (for ((set i 1)
              (lt i n)
              (set i (add i 1)))
            (if (lt (load (ptradd arr i)) (load (ptradd arr idx)))
                (set idx i)))
        (ret idx))

    (define ((insertion-sort void) (arr (ptr int)) (len int))
        (declare min-idx int)
        (declare i int)
        (declare rest-arr (ptr int))

        (for ((set i 0)
              (lt i (sub len 1))
              (set i (add i 1)))
            (set rest-arr (ptradd arr i))
            (set min-idx (call min rest-arr (sub len i)))
            (if (not (eq min-idx 0))
                (call swap
                    rest-arr
                    (ptradd rest-arr min-idx))))
        (ret))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare input (ptr int))
        (declare len int)
        (declare i int)

        (set len (sub argc 1))
        (set input (alloc int len))

        (for ((set i 0)
              (lt i len)
              (set i (add i 1)))
            (store
                (ptradd input i)
                (call atoi (load (ptradd argv (add i 1))))))

        (call insertion-sort input len)

        (for ((set i 0)
              (lt i len)
              (set i (add i 1)))
            (call print (load (ptradd input i))))
        (ret)))

