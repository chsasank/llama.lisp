(c-lisp
    (define ((fprint float) (n  float) ))

    (define ((putchar int) (n int)))

    (define ((initialise void) (x (ptr float)) (w (ptr float)) (column int) (row int))
        (declare row_float float)
        (declare column_float float)
        (declare i int)
        (declare j int)
        (set row_float 0.0)
        (set column_float 0.0)
        (for ((set i 0) (lt i row) (set i (add i 1)) )
            (store (ptradd x i) row_float)
            (for ((set j 0) (lt j column) (set j (add j 1)))
                (store (ptradd w (add j (mul i row))) (fadd column_float row_float ))
                (set column_float (fadd column_float 1.0))
            )
            (set row_float (fadd row_float 1.0))
            (set column_float 0.0)
        )
        (ret)
    )


    (define ((matmul_print void) (arr (ptr float)) (len int))
        (declare i int)
        (for ((set i 0) (lt i len) (set i (add i 1))) 
            (call fprint (load (ptradd arr i)))
        )
        (ret) 
    ) 

    (define ((mat_mul void) (xout (ptr float)) (x (ptr float)) (w (ptr float)) (n int) (d int)) 
        (declare i int)
        (for ((set i 0) (lt i d) (set i (add i 1)))
            (declare val float)
            (set val 0.0)
            (declare j int)
            (for ((set j 0) (lt j n) (set j (add j 1)))
            (set val (fadd val (fmul (load (ptradd w (add (mul i n) j))) (load (ptradd x j)))))
            )
            (store (ptradd xout i) val)
            (call fprint (load (ptradd xout i)))
        )
            (ret)  
    )


    (define ((main void))
        (declare xout (ptr float))
        (declare x (ptr float))
        (declare w (ptr float))
        (declare n int)
        (declare d int)
        (set xout (alloc float 10))
        (set x (alloc float 10))
        (set w (alloc float 100))
        (set n 10)
        (set d 10)
        (call initialise x w n d)
        (call mat_mul xout x w n d)
        (ret) 
    )
)