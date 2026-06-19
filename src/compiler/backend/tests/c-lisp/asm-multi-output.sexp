;; Multi-output inline asm returning a struct, accessed with extractvalue.
(c-lisp
    (define-struct pair (lo int) (hi int))
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare p (struct pair))
        (set p (asm (struct pair) "movl $$11, $0; movl $$22, $1;" "=r,=r"))
        (call print (extractvalue p 0))
        (call print (extractvalue p 1))
        (ret)))
