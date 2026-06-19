;; Function that builds and returns a struct via insertvalue.
(c-lisp
    (define-struct point (x int) (y int))
    (define ((print int) (n int)))

    (define ((make-point (struct point)) (x int) (y int))
        (declare p (struct point))
        (set p (insertvalue p x 0))
        (set p (insertvalue p y 1))
        (ret p))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare p (struct point))
        (set p (call make-point 7 8))
        (call print (extractvalue p 0))
        (call print (extractvalue p 1))
        (ret)))