(c-lisp
    (define ((fprint float) (n float)))
    
    (define ((print int) (n int)))

    (define ((print_char int) (n int)))

    (define ((random_matrix int) (matrix (ptr float)) (rows int) (cols int)))

    (define ((print_matrix int) (matrix (ptr float)) (rows int) (cols int)))

    (define ((ref_mult int) (A (ptr float)) (B (ptr float)) (C (ptr float)) (m int) (n int) (k int)))

    (define ((add_dot void)
                (A (ptr float))
                (B (ptr float))
                (C (ptr float))
                (m int)
                (n int)
                (k int)
            )
        (ret)

    )

    (define ((mmult2 void)
                (A (ptr float))
                (B (ptr float))
                (C (ptr float))
                (m int)
                (n int)
                (k int)
            )

        ; TODO : code to multiply matrix inspired by MMult2.c from https://github.com/flame/how-to-optimize-gemm.git

        (ret)

        
    )

    (define ((main void))
        (declare A    (ptr float))
        (declare B    (ptr float))
        (declare C    (ptr float))
        
        (declare m int)
        (declare n int)
        (declare k int)

        (declare size_a int)
        (declare size_b int)
        (declare size_c int)

        (set m 1)
        (set n 3)
        (set k 2)
        
        (set size_a (mul m k))
        (set size_b (mul k n))
        (set size_c (mul m n))
        
        (set A (alloc float size_a))
        (set B (alloc float size_b))
        (set C (alloc float size_c))

        (call random_matrix A m k)
        (call random_matrix B k n)

        (call ref_mult A B C m n k)

        (call print_matrix A m k)
        (call print_matrix B k n)
        (call print_matrix C m n)

        (ret)
    )
)