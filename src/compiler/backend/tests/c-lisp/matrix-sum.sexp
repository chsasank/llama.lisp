(c-lisp
    (define ((malloc (ptr int)) (bytes int)))
    (define ((free void) (arr (ptr int))))
    (define ((print int) (n int)))

    (define ((index int) (i int) (j int) (len int))
        (ret (add (mul i len) j)))

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

    (define ((mat-add (ptr int)) (a (ptr int)) (b (ptr int)) (len int))
        (declare i int)
        (declare j int)
        (declare idx int)

        (declare res (ptr int))
        (set res (call malloc (mul (mul len len) 4)))

        (for ((set i 0)
              (lt i len)
              (set i (add i 1)))
            (for ((set j 0)
                  (lt j len)
                  (set j (add j 1)))
                (set idx (call index i j len))
                (store
                    (ptradd res idx)
                    (add
                        (load (ptradd a idx))
                        (load (ptradd b idx))))))
        (ret res))

    (define ((main void))
        (declare in1 (ptr int))
        (declare in2 (ptr int))
        (declare out (ptr int))

        (set in1 (alloc int 16))
        (call mat-cons in1 4)
        (set in2 (alloc int 16))
        (call mat-cons in2 4)

        (set out (call mat-add in1 in2 4))

        (declare i int)
        (for ((set i 0)
              (lt i 16)
              (set i (add i 1)))
            (call print (load (ptradd out i))))
        (call free out)
        (ret)))
