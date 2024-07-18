(c-lisp
    (define ((main void))
        (declare a (ptr int))
        (set a (alloc int 3))
        (store a 5 6 7)))
