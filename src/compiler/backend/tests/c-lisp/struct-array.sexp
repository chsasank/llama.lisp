;; Array of structs.
(c-lisp
    (define-struct node (val int))
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare arr (arr 3 (struct node)))
        (declare i int)

        (for ((set i 0) (lt i 3) (set i (add i 1)))
            (store (sptradd (aptradd arr i) val) (mul i 10)))

        (for ((set i 0) (lt i 3) (set i (add i 1)))
            (call print (load (sptradd (aptradd arr i) val))))

        (ret)))
