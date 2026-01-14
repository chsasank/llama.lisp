(cuda-lisp
    (define ((llvm.memcpy.p0i8.p0i8.i64 void) (dest (ptr int8)) (src (ptr int8)) (len int64) (isvolatile bool)))
    (define-inline-brilisp ((memcpy void) (dst (ptr int8)) (src (ptr int8)) (n int))
        (set (n_64 int64) (sext n int64))
        (set (tmp void) (call llvm.memcpy.p0i8.p0i8.i64 dst src n_64 #f))
        (ret))
    (define-kernel ((kernel void)
             (d-in (ptr int))
             (d-out (ptr int))
             (n int))
        (declare idx int)
        (set idx (add (mul (bid.x) (bdim.x)) (tid.x)))

        (declare i int)
        (for ((set i idx) (lt i (div n 4)) (set i (add i (mul (bdim.x) (gdim.x)))))
            (call memcpy
                (bitcast (ptradd d-out (mul i 4)) (ptr int8))
                (bitcast (ptradd d-in (mul i 4)) (ptr int8))
                16))

        (ret)))
