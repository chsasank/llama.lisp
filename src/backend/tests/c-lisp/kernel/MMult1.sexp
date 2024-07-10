(c-lisp

    (define ((__MMult1 void) (A (ptr float))
                            (B (ptr float))    
                            (C (ptr float))    
                            (m int)    
                            (n int)    
                            (k int))

        (declare i int)
        (declare j int)
        (declare p int)

        (for ((set j 0) (lt j n) (set j (add j 1)))
            (for ((set i 0) (lt i m) (set i (add i 1)))
                (declare sum float)
                (set sum 0.0)
                (for ((set p 0) (lt p k) (set p (add p 1)))
                    ; sum += A[p * m + i] * B[j * k + p]
                    (set sum (fadd
                        (fmul 
                            (load (ptradd A (add (mul p m) i)))
                            (load (ptradd B (add (mul j k) p))))
                        sum)))
                (store (ptradd C (add (mul j m) i)) sum)))
        (ret)))

