;; Nested struct types.
(c-lisp
    (define-struct point (x int) (y int))
    (define-struct box (origin (struct point)) (w int) (h int))

    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare b (struct box))

        (store (sptradd (sptradd (ptr-to b) origin) x) 3)
        (store (sptradd (sptradd (ptr-to b) origin) y) 4)
        (store (sptradd (ptr-to b) w) 10)
        (store (sptradd (ptr-to b) h) 20)

        (call print (load (sptradd (sptradd (ptr-to b) origin) x)))
        (call print (load (sptradd (sptradd (ptr-to b) origin) y)))
        (call print (load (sptradd (ptr-to b) w)))
        (call print (load (sptradd (ptr-to b) h)))
        (ret)))
