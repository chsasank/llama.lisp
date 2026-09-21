;; Select between struct values based on a boolean condition.
(c-lisp
    (define-struct pair (lo int) (hi int))
    (define ((print int) (n int)))

    (define ((make-pair (struct pair)) (lo int) (hi int))
        (declare p (struct pair))
        (set p (insertvalue p lo 0))
        (set p (insertvalue p hi 1))
        (ret p))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare a (struct pair))
        (declare b (struct pair))
        (declare c (struct pair))
        (set a (call make-pair 1 2))
        (set b (call make-pair 3 4))
        (set c (select #t a b))
        (call print (extractvalue c 0))
        (call print (extractvalue c 1))
        (set c (select #f a b))
        (call print (extractvalue c 0))
        (call print (extractvalue c 1))
        (ret)))