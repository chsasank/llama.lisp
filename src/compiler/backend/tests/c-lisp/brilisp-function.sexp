;; Plain define-brilisp function, independent of asm/intrinsics.
(c-lisp
    (define ((print int) (n int)))

    (define-brilisp ((dbl int) (x int))
        (set (res int) (mul x 2))
        (ret res))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (call dbl 5))
        (call print (call dbl -3))
        (ret)))
