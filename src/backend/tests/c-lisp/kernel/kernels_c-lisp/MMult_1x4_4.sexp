(c-lisp

    (define ((__add_dot1x4 void) (k int)
                                (a (ptr float))
                                (lda int)
                                (b (ptr float))
                                (ldb int)
                                (c (ptr float))
                                (ldc int))
        (declare p int)

        ,@(init_c c ldc)
        ,@(elemental_muladd_1x4 k a b c lda ldb ldc)
        
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
