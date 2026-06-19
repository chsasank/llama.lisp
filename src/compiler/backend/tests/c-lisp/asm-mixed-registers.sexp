;; Mixed register-class outputs: one int (=r) and one float (=x).
(c-lisp
    (define-struct mixed (i int) (f float))
    (define ((print int) (n int)))
    (define ((fprint float) (n float)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare s (struct mixed))
        (set s (asm "movl $$7, $0; xorps $1, $1;" "=r,=x"))
        (call print (extractvalue s 0))
        (call fprint (extractvalue s 1))
        (ret)))
