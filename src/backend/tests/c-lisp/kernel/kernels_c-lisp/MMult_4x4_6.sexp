(c-lisp
    (define ((__add_dot4x4 void) (k int)
                                (a (ptr float))
                                (lda int)
                                (b (ptr float))
                                (ldb int)
                                (c (ptr float))
                                (ldc int))
        (declare p int)
        (declare i int)
        (declare j int)

        (for ((set p 0) (lt p k) (set p (add p 1)))
            (for ((set i 0) (lt i 4) (set i (add i 1)))
                (for ((set j 0) (lt j 4) (set j (add j 1)))
                    (store (ptradd c (add j (mul i ldc))) 
                        (fadd (load (ptradd c (add j (mul i ldc))))
                            (fmul (load (ptradd a (add p (mul i lda))))
                                  (load (ptradd b (add j (mul p ldb))))))))))
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

        (set lda k)
        (set ldb n)
        (set ldc n)

        ; Initialize C to 0
        (for ((set i 0) (lt i m) (set i (add i 1)))
            (for ((set j 0) (lt j n) (set j (add j 1)))
                (store (ptradd c (add j (mul i ldc))) 0.0)))

        (for ((set j 0) (lt j n) (set j (add j 4)))
            (for ((set i 0) (lt i m) (set i (add i 4)))
                (call __add_dot4x4 
                        k
                        (ptradd a (mul i lda))
                        lda
                        (ptradd b j)
                        ldb
                        (ptradd c (add j (mul i ldc)))
                        ldc))) 
        (ret)))
