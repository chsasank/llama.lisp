;; Copy a struct value from one variable to another.
(c-lisp
    (define-struct point (x int) (y int))
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare a (struct point))
        (declare b (struct point))

        (store (sptradd (ptr-to a) x) 1)
        (store (sptradd (ptr-to a) y) 2)

        (set b a)

        (call print (load (sptradd (ptr-to b) x)))
        (call print (load (sptradd (ptr-to b) y)))

        ; Modify b, ensure a unchanged
        (store (sptradd (ptr-to b) x) 99)
        (call print (load (sptradd (ptr-to a) x)))
        (ret)))
