;; Build a struct value using insertvalue.
(c-lisp
    (define-struct pair (lo int) (hi int))
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare p (struct pair))
        (set p (insertvalue p 11 0))
        (set p (insertvalue p 22 1))
        (call print (extractvalue p 0))
        (call print (extractvalue p 1))
        (ret)))