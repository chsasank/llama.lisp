(c-lisp
    (declare ((print int) (n int)))

    (define ((swap void) (a (ptr int)) (b (ptr int)))
        (declare tmp int)
        (set tmp (load b))
        (store b (load a))
        (store a tmp)
        (ret))

    (define ((index int) (i int) (j int) (len int))
        (ret (add (mul i len) j)))

    (define ((transpose void) (mat (ptr int)) (len int))
        (declare i int)
        (declare j int)

        (for ((set i 0)
              (lt i len)
              (set i (add i 1)))
            (for ((set j (add i 1))
                  (lt j len)
                  (set j (add j 1)))
                (call swap (ptradd mat (call index i j len)) (ptradd mat (call index j i len)))))
        (ret))

    (define ((mat-cons void) (arr (ptr int)) (len int))
        (declare i int)
        (declare j int)

        (for ((set i 0)
              (lt i len)
              (set i (add i 1)))
            (for ((set j 0)
                  (lt j len)
                  (set j (add j 1)))
                (store
                    (ptradd arr (call index i j len))
                    (add 11 (add (mul i 10) j)))))
        (ret))

    (define ((arr-print void) (arr (ptr int)) (len int))
        (declare i int)

        (for ((set i 0)
              (lt i len)
              (set i (add i 1)))
            (call print (load (ptradd arr i))))
        (ret))

    (define ((main void))
        (declare input (ptr int))

        (set input (alloc int 25))
        (call mat-cons input 5)
        (call transpose input 5)
        (call arr-print input 25)
        (ret)))
