;; Native floating-point arithmetic: fadd, fsub, fmul, fdiv.
(c-lisp
    (define ((fprint float) (n float)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare a float)
        (declare b float)
        (set a 6.0)
        (set b 2.5)

        (call fprint (fadd a b)) ; 8.5
        (call fprint (fsub a b)) ; 3.5
        (call fprint (fmul a b)) ; 15.0
        (call fprint (fdiv a b)) ; 2.4
        (ret)))
