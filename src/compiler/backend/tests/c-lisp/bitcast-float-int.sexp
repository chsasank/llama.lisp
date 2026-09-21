;; Bitcast between float and int.
(c-lisp
    (define ((print int) (n int)))
    (define ((fprint float) (n float)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare f float)
        (declare bits int)

        (set f 1.0)
        (set bits (bitcast f int))
        (call print bits)

        (set f (bitcast bits float))
        (call fprint f)

        (set bits 1065353216)
        (set f (bitcast bits float))
        (call fprint f)

        (ret)))
