(c-lisp

    (define ((__add_dot1x4 void) (k int)
                                (a (ptr float))
                                (lda int)
                                (b (ptr float))
                                (ldb int)
                                (c (ptr float))
                                (ldc int))
        (declare p int)

        (store (ptradd c (add 0 (mul 0 ldc))) 0.0)
        (store (ptradd c (add 0 (mul 1 ldc))) 0.0)
        (store (ptradd c (add 0 (mul 2 ldc))) 0.0)
        (store (ptradd c (add 0 (mul 3 ldc))) 0.0)
        

        (for ((set p 0) (lt p k) (set p (add p 1)))
            (store (ptradd c (add 0 (mul 0 ldc)))
                (fadd (load (ptradd c (add 0 (mul 0 ldc))))
                      (fmul (load (ptradd a (add 0 (mul p lda))))
                            (load (ptradd b (add p (mul 0 ldb))))))))

        (for ((set p 0) (lt p k) (set p (add p 1)))
            (store (ptradd c (add 0 (mul 1 ldc)))
                (fadd (load (ptradd c (add 0 (mul 1 ldc))))
                      (fmul (load (ptradd a (add 0 (mul p lda))))
                            (load (ptradd b (add p (mul 1 ldb))))))))

        (for ((set p 0) (lt p k) (set p (add p 1)))
            (store (ptradd c (add 0 (mul 2 ldc)))
                (fadd (load (ptradd c (add 0 (mul 2 ldc))))
                      (fmul (load (ptradd a (add 0 (mul p lda))))
                            (load (ptradd b (add p (mul 2 ldb))))))))

        (for ((set p 0) (lt p k) (set p (add p 1)))
            (store (ptradd c (add 0 (mul 3 ldc)))
                (fadd (load (ptradd c (add 0 (mul 3 ldc))))
                      (fmul (load (ptradd a (add 0 (mul p lda))))
                            (load (ptradd b (add p (mul 3 ldb))))))))
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