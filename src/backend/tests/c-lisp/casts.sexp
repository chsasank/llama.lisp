;; ARGS: 12345

(c-lisp
    (declare ((print int) (n int)))
    (declare ((fprint float) (n float)))
    (declare ((ptr_print (ptr int)) (p (ptr int))))

    (declare ((powf float) (x float) (y float)))
    (declare ((pow double) (x double) (y double)))
    (declare ((putchar int) (c int)))

    (define ((print16 void) (n int16))
        (call print (sext n int32)))
    (define ((newline void))
        (call putchar 10)) ; ASCII newline

    ;; Break up an int32 into 2 int16, using pointer casts
    ;; tests bitcast
    (define ((bitcast-test void))
        (declare num-32 (ptr int))
        (set num-32 (alloc int 1))
        (store num-32 196612) ; 196612 = (3 << 16) + 4

        (declare num-16 (ptr int16))
        (set num-16 (bitcast num-32 (ptr int16)))

        (call print16 (load (ptradd num-16 1))) ; higher 16 bits
        (call print16 (load num-16))) ; lower 16 bits

    ;; Carry out a computation that generates an intermediate
    ;; too big to fit in fp32.
    ;; tests fpext, fptrunc
    (define ((fp-conv void))
        (declare big-exp float)
        (set big-exp (call powf 2.0 6.0))
        (declare small-fp float)
        (set small-fp (call powf 2.0 big-exp))
        ; `small-fp` is almost too big for float32
        ; 2^(2^7) cannot be expressed as float32

        ; This won't work; too big for fp32
        (call fprint (call powf
            small-fp
            2.0))

        ; Not too big for fp64
        (declare big-fp double)
        (set big-fp (call pow
            (fpext small-fp double)
            (fpext 2.0 double)))

        ; Verify that numbers are correct
        (call fprint (fptrunc
            (call pow big-fp (fpext 0.5 double))
            float)))

    ;; Carry out a computation that generates an intermediate
    ;; too big to fit in int8
    ;; tests (s|z)ext, trunc
    (define ((int-conv void))
        (declare big int)
        ;; Almost too big for int8
        ;; 256 cannot fit in int8
        (declare small-int int8)
        (set small-int (trunc 255 int8))

        ; This won't work; too big for int8
        (call print16 (sext
            (mul small-int (trunc 2 int8))
            int16))

        ; Not too big for int16
        (declare big-int int16)
        (call print16 (mul
            (zext small-int int16)
            (trunc 2 int16))))

    ;; tests (s|u)itofp, fpto(s|u)i
    (define ((int-fp void))
        (declare fnum float)

        (set fnum (sitofp -3 float))
        (call fprint fnum)
        (call print (fptosi fnum int))

        (set fnum (uitofp 7 float))
        (call fprint fnum)
        (call print (fptoui fnum int)))

    ;; Print the middle character of a string, using pointer arithmetic
    ;; tests ptrtoint, inttoptr
    (define ((str-mid void) (start-ptr (ptr int8)))
        (declare end-ptr (ptr int8))
        (set end-ptr start-ptr)
        (while (ne (load end-ptr) (trunc 0 int8))
            (set end-ptr (ptradd end-ptr 1)))

        (declare sum int64)
        (set sum (add
            (ptrtoint end-ptr int64)
            (ptrtoint start-ptr int64)))

        (declare mid-ptr (ptr int8))
        (set mid-ptr(inttoptr
            (div sum (sext 2 int64))
            (ptr int8)))
        (call putchar (zext (load mid-ptr) int)))

    ;; Some more edgecases
    (define ((edgecases void))
        (declare num32 int)
        (declare num8 int8)
        (set num32 -255)
        (call ptr_print (inttoptr num32 (ptr int)))

        (set num8 (trunc -1 int8))
        (call print (zext num8 int))
        (call print (sext num8 int))

        (set num32 127)
        (set num8 (trunc num32 int8))
        (call print (sext num8 int)))

    (define ((main void) (argc int) (argv (ptr (ptr int8))))
        (call bitcast-test)
        (call newline)
        (call fp-conv)
        (call newline)
        (call int-conv)
        (call newline)
        (call int-fp)
        (call newline)
        (call str-mid (load (ptradd argv 1)))
        (call newline)
        (call edgecases)
        (call newline)))
