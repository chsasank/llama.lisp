;; C-Lisp function returning a struct by value.
(c-lisp
    (define-struct point (x int) (y int))
    (define ((print int) (n int)))

    (define ((make-point (struct point)) (x int) (y int))
        (declare p (struct point))
        (store (sptradd (ptr-to p) x) x)
        (store (sptradd (ptr-to p) y) y)
        (ret p))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare p (struct point))
        (set p (call make-point 3 4))
        (call print (load (sptradd (ptr-to p) x)))
        (call print (load (sptradd (ptr-to p) y)))
        (ret)))
