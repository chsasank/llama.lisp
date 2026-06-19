;; Inline function wrapping a single-output asm add.
(c-lisp
    (define ((print int) (n int)))

    (define-inline ((iadd int) (a int) (b int))
        (declare res int)
        (set res (asm int "movl $1, $0; addl $2, $0;" "=r,r,r" a b))
        (ret res))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (call iadd 3 4))
        (ret)))
