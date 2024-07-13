(c-lisp
    (define ((__add_dot void) (k int)
                            (a (ptr float))
                            (incx int)
                            (b (ptr float))
                            (c (ptr float)))
        (declare p int)
        (declare a_value float)
        (declare b_value float)

        (for ((set p 0) (lt p k) (set p (add p 1)))
            (set a_value (load (ptradd a (mul p incx))))
            (set b_value (load (ptradd b p)))
            (store c (fadd (load c) (fmul a_value b_value))))
    (ret))

    (define ((__MMult2 void) (m int)
                             (n int)
                             (k int)
                             (lda int)
                             (ldb int)
                             (ldc int)
                             (a (ptr float))
                             (b (ptr float))
                             (c (ptr float)))
        (declare i int)
        (declare j int)

        (for ((set j 0) (lt j n) (set j (add j 4)))
            (for ((set i 0) (lt i m) (set i (add i 1)))

                (call __add_dot 
                    k 
                    (ptradd a (add i (mul 0 lda))) 
                    lda 
                    (ptradd b (add 0 (mul (add j 0) ldb)))
                    (ptradd c (add i (mul (add j 0) ldc))))

                (call __add_dot 
                    k 
                    (ptradd a (add i (mul 0 lda))) 
                    lda 
                    (ptradd b (add 0 (mul (add j 1) ldb)))
                    (ptradd c (add i (mul (add j 1) ldc))))

                (call __add_dot 
                    k 
                    (ptradd a (add i (mul 0 lda))) 
                    lda 
                    (ptradd b (add 0 (mul (add j 2) ldb)))
                    (ptradd c (add i (mul (add j 2) ldc))))

                (call __add_dot 
                    k 
                    (ptradd a (add i (mul 0 lda))) 
                    lda 
                    (ptradd b (add 0 (mul (add j 3) ldb)))
                    (ptradd c (add i (mul (add j 3) ldc))))))
    (ret)))