;; Use a struct produced by one asm as input to another asm (via extractvalue).
(c-lisp
    (define-struct pair (lo int) (hi int))
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare p (struct pair))
        (declare sum int)
        (set p (asm (struct pair) "movl $$5, $0; movl $$6, $1;" "=r,=r"))
        (set sum (asm int "movl $1, $0; addl $2, $0;" "=r,r,r" (extractvalue p 0) (extractvalue p 1)))
        (call print sum)
        (ret)))
