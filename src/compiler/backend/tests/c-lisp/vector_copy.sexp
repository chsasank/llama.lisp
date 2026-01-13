(c-lisp
    (define ((fprint float) (n float)))
    (define ((fprint float) (n float)))
    (define ((llvm.memcpy.p0i8.p0i8.i64 void) (dest (ptr int8)) (src (ptr int8)) (len int64) (isvolatile bool)))
    (define-inline-brilisp ((memcpy void) (dst (ptr int8)) (src (ptr int8)) (n int))
        (set (n int64) (sext n int64))
        (set (tmp void) (call llvm.memcpy.p0i8.p0i8.i64 dst src n #f))
        (ret))
    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare n int)
        (set n 10)

        (declare src (ptr float))
        (set src (alloc float n))

        (declare dst (arr 10 float))

        ; init arrs
        (declare idx int)
        (for ((set idx 0) (lt idx n) (set idx (add idx 1)))
            (store (ptradd src idx) (sitofp idx float))
            (store (aptradd dst idx) 0.0))

        ; copy first 4 values
        (call memcpy
            (bitcast (aptradd dst 0) (ptr int8))
            (bitcast src (ptr int8)) 
            16)

        ; copy next 4 values
        (call memcpy
            (bitcast (aptradd dst 4) (ptr int8))
            (bitcast (ptradd src 4) (ptr int8))
            16)

        (for ((set idx 0) (lt idx n) (set idx (add idx 1)))
            (call fprint (load (aptradd dst idx))))

        (ret)))
