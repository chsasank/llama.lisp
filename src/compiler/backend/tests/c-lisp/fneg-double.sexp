;; Floating-point negation (fneg) on double values.
(c-lisp
    (define ((dprint double) (n double)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare x double)
        (set x (fpext 4.25 double))
        (call dprint (fneg (fpext 1.5 double)))
        (call dprint (fneg x))
        (call dprint (fneg (fneg x)))
        (ret)))