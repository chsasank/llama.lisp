;; Zero-output inline asm with a side effect: write through a pointer input.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare x int)
        (set x 0)
        (asm void "movl $$7, ($0)" "r" (ptr-to x))
        (call print x)
        (ret)))
