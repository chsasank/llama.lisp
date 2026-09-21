;; Single-output inline asm returning an int.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare x int)
        (set x (asm int "movl $$42, $0" "=r"))
        (call print x)
        (ret)))
