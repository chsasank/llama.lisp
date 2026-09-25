(c-lisp
    (define ((__add_dot void) (k int)
                            (a (ptr float))
                            (incx int)
                            (b (ptr float))
                            (c (ptr float)))
        (declare p int)
        (declare a_value float)
        (declare b_value float)
        (declare acc float)
        
        ;; Initialize accumulator to zero
        (set acc 0.0)

        (for ((set p 0) (lt p k) (set p (add p 1)))
            (set a_value (load (ptradd a (mul p incx))))
            (set b_value (load (ptradd b p)))
            (set acc (fadd acc (fmul a_value b_value))))

        ;; Store the accumulated result in c
        (store c acc)
    (ret))

    (define ((__kernel void)(a (ptr float))
                            (b (ptr float))
                            (c (ptr float))
                            (n int)
                            (k int)
                            (m int)) 
                            
        (declare i int)
        (declare j int)

        (declare lda int)
        (declare ldb int)
        (declare ldc int)

        (set lda m)
        (set ldb n)
        (set ldc k)

        (for ((set j 0) (lt j n) (set j (add j 4)))
            (for ((set i 0) (lt i m) (set i (add i 1)))
                ,@(call_add_dot a b c lda ldb ldc 0 i j k)
                ,@(call_add_dot a b c lda ldb ldc 1 i j k)
                ,@(call_add_dot a b c lda ldb ldc 2 i j k)
                ,@(call_add_dot a b c lda ldb ldc 3 i j k)))
    (ret)))
