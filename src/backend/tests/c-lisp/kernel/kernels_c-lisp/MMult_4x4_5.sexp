(c-lisp
    ;; Function to add dot products for a 4x4 block
    (define ((__add_dot4x4 void)
            (k int)
            (a (ptr float))
            (lda int)
            (b (ptr float))
            (ldb int)
            (c (ptr float))
            (ldc int))

        (declare p int)

        ,@(init_c c ldc 0)
        ,@(init_c c ldc 1)
        ,@(init_c c ldc 2)
        ,@(init_c c ldc 3)

        (for ((set p 0) (lt p k) (set p (add p 1)))
            ,@(elemental_muladd_4x4 a b c lda ldb ldc p 0)
            ,@(elemental_muladd_4x4 a b c lda ldb ldc p 1)
            ,@(elemental_muladd_4x4 a b c lda ldb ldc p 2)
            ,@(elemental_muladd_4x4 a b c lda ldb ldc p 3))

        (ret))

    (define ((__kernel void)
            (a (ptr float))
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
            (for ((set i 0) (lt i m) (set i (add i 4)))
                (call __add_dot4x4 k (ptradd a (add i (mul 0 lda))) lda (ptradd b (mul j ldb)) ldb (ptradd c (add i (mul j ldc))) ldc)))

        (ret)))

