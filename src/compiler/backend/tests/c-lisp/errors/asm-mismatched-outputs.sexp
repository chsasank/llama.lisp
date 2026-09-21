(c-lisp
    (define-struct pair (lo int) (hi int))
    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare p (struct pair))
        (set p (asm (struct pair) "movl $$42, $$0" "=r"))
        (ret)))
