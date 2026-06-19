;; Inline function returning a struct produced by multi-output asm.
(c-lisp
    (define-struct pair (lo int) (hi int))
    (define ((print int) (n int)))

    (define-inline ((make-pair (struct pair)) (a int) (b int))
        (declare p (struct pair))
        (set p (asm "movl $2, $0; movl $3, $1;" "=r,=r,r,r" a b))
        (ret p))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare p (struct pair))
        (set p (call make-pair 11 22))
        (call print (extractvalue p 0))
        (call print (extractvalue p 1))
        (ret)))
