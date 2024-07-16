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

    (define ((__add_dot1x4 void) (k int)
                                (a (ptr float))
                                (lda int)
                                (b (ptr float))
                                (ldb int)
                                (c (ptr float))
                                (ldc int))
        (call __add_dot
            k
            a
            lda
            (ptradd b (mul 0 ldb))
            (ptradd c (mul 0 ldc)))
        
        (call __add_dot
            k
            a
            lda
            (ptradd b (mul 1 ldb))
            (ptradd c (mul 1 ldc)))

        (call __add_dot
            k
            a
            lda
            (ptradd b (mul 2 ldb))
            (ptradd c (mul 2 ldc)))

        (call __add_dot
            k
            a
            lda
            (ptradd b (mul 3 ldb))
            (ptradd c (mul 3 ldc)))
    (ret))

    (define ((__kernel void) (a (ptr float))
                                  (b (ptr float))
                                  (c (ptr float))
                                  (m int)
                                  (n int)
                                  (k int)) 
                            
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
                (call __add_dot1x4
                 k 
                 (ptradd a i) 
                 lda 
                 (ptradd b (mul j ldb))
                 ldb 
                 (ptradd c (add i (mul j ldc))) 
                 ldc)))
    (ret)))
