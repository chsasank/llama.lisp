;; Inline functions wrapping side-effect asm for memory load/store.
(c-lisp
    (define ((print int) (n int)))

    (define-inline ((istore void) (p (ptr int)) (v int))
        (set (tmp void) (asm "movl $1, ($0)" "r,r" p v))
        (ret))

    (define-inline ((iload int) (p (ptr int)))
        (declare v int)
        (set v (asm "movl ($1), $0" "=r,r" p))
        (ret v))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare x int)
        (set x 0)
        (call istore (ptr-to x) 42)
        (call print (call iload (ptr-to x)))
        (ret)))
