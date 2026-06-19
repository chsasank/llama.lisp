;; Select between floats based on a boolean condition.
(c-lisp
    (define ((fprint float) (n float)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call fprint (select #t 1.5 2.5))
        (call fprint (select #f 1.5 2.5))
        (call fprint (select (flt 1.0 2.0) 10.5 20.5))
        (call fprint (select (fgt 1.0 2.0) 10.5 20.5))
        (ret)))