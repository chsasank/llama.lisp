;; Function returning a pointer (passed in, to avoid alloca escape UB).
(c-lisp
    (define ((print int) (n int)))

    (define ((identity-ptr (ptr int)) (p (ptr int)))
        (ret p))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare x int)
        (declare p (ptr int))
        (set x 99)
        (set p (ptr-to x))
        (set p (call identity-ptr p))
        (call print (load p))
        (ret)))
