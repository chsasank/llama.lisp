;; Plain define-inline function (no asm).
(c-lisp
    (define ((print int) (n int)))

    (define-inline ((square int) (x int))
        (ret (mul x x)))

    (define-inline ((add int) (a int) (b int))
        (ret (add a b)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (call square 5))
        (call print (call add 3 4))
        (ret)))
