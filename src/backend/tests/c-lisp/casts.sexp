(c-lisp
    (define ((print int) (n int)))
    (define ((fprint float) (n float)))

    (define ((powf float) (x float) (y float)))
    (define ((pow double) (x double) (y double)))
    (define ((putchar int) (c int)))

    (define ((print16 void) (n int16))
        (call print (sext n int32)))

    ; bitcast
    (define ((bitcast-test void))
        (declare num-32 (ptr int))
        (set num-32 (alloc int 1))
        (store num-32 196612) ;  = (3 << 16) + 4

        (declare num-16 (ptr int16))
        (set num-16 (bitcast num-32 (ptr int16)))

        (call print16 (load (ptradd num-16 1)))
        (call print16 (load num-16)))

    ; fpext, fptrunc
    (define ((fp-conv void))
        (declare big-exp float)
        ;; Almost too big for float32
        ;; 2^(2^7) cannot be expressed as float32
        (set big-exp (call powf 2.0 6.0))

        (declare small-fp float)
        (set small-fp (call powf 2.0 big-exp))

        (declare big-fp double)
        (set big-fp (call pow
            (fpext small-fp double)
            (fpext 2.0 double)))

        (call fprint (call powf
            small-fp
            2.0))

        (call fprint (fptrunc
            (call pow big-fp (fpext 0.5 double))
            float)))

    ; (s|z)ext, trunc
    (define ((int-conv void))
        (declare big int)
        ;; Almost too big for int8
        ;; 256 cannot fit in int8
        (declare small-int int8)
        (set small-int (trunc 255 int8))

        (call print16 (sext
            (mul small-int (trunc 2 int8))
            int16))

        (declare big-int int16)
        (call print16 (mul
            (zext small-int int16)
            (trunc 2 int16))))

    ; (s|u)itofp, fpto(s|u)i
    (define ((int-fp void))
        (declare fnum float)

        (set fnum (sitofp -3 float))
        (call fprint fnum)
        (call print (fptosi fnum int))

        (set fnum (uitofp 7 float))
        (call fprint fnum)
        (call print (fptoui fnum int)))



    (define ((main void))
        (call bitcast-test)
        (call putchar 10)
        (call fp-conv)
        (call putchar 10)
        (call int-conv)
        (call putchar 10)
        (call int-fp)))
