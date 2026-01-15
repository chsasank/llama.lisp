(cuda-lisp
    (define-struct int4 (x int) (y int) (z int) (w int))
    (define-inline-brilisp ((memcpy4 void) (dst (ptr int8)) (src (ptr int8)))
        (set (temp (struct int4))
            (asm "ld.global.v4.b32 {$0, $1, $2, $3}, [$4]; st.global.v4.b32 [$5], {$0, $1, $2, $3};"
                "=&r,=&r,=&r,=&r,l,l" src dst))
        (ret))

    (define-kernel ((kernel void)
             (d-in (ptr int))
             (d-out (ptr int))
             (n int))
        (declare idx int)
        (set idx (add (mul (bid.x) (bdim.x)) (tid.x)))

        (declare i int)
        (for ((set i idx) (lt i (div n 4)) (set i (add i (mul (bdim.x) (gdim.x)))))
            (call memcpy4
                (bitcast (ptradd d-out (mul i 4)) (ptr int8))
                (bitcast (ptradd d-in (mul i 4)) (ptr int8))))
        (ret)))
