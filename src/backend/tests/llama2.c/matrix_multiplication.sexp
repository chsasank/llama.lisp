
(c-lisp
	(define ((mat-cons void) (arr (ptr float)) (len int))
		(declare (n int))
		(declare (m float))
		(set m 71.5)
		(for ((set n 0) (lt n len) (set n (add n 1)) )
			(store (ptradd arr n) m )
		)
		(ret)
	)
	(define ((fprint float) (n  float) ))

	(define ((matmul_print void) (arr (ptr float)) (len int))
		(declare (i int))
		(for ((set i 0) (lt i len) (set i (add i 1))) 
			(call fprint (load (ptradd arr i)))
		)
		(ret) 
	) 


 ;x is 10 , xout and w is 100 . d is 10 . n is 10
	(define ((mat_mul void) (xout (ptr float)) (x (ptr float)) (w (ptr float)) (n int) (d int)) 
		(declare (i int))
		(for ( (set i 0) (lt i d) (set i (add i 1)) )
			(declare (val float))
			(set val 0.0)
			(declare (j int))
			(for ((set j 0) (lt j n) (set j (add j 1)))
			   (set val (fadd val (fmul (load (ptradd w (add (mul i n) j))) (load (ptradd x j)))))
			)
			(store (ptradd xout i) val)
		)
		(ret)  
	)


	(define ((main void))
		(declare (xout (ptr float)))
		(declare (x (ptr float)))
		(declare (w (ptr float)))
		(declare (n int ))
		(declare (d int))


		(set xout (alloc float 100))
		(set x (alloc float 10))
		(set w (alloc float 100))
		(set n 10)
		(set d 10)
		(call mat-cons w 100)
		(call mat-cons x 10)
		(call mat_mul xout x w n d)
		(call matmul_print xout 100)
		;(call fprint xout)
		(ret) 
	)
)


