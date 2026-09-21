;; Floating-point negation (fneg) on literals and variables.
(c-lisp
    (define ((fprint float) (n float)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare x float)
        (set x 3.5)
        (call fprint (fneg 2.0))
        (call fprint (fneg x))
        (call fprint (fneg (fneg x)))
        (ret)))