;; Global array with a const initializer.
(c-lisp
    (define ((print int) (n int)))

    (define-global (a (arr 3 int)) (const (1 2 3)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (load (aptradd a 0)))
        (call print (load (aptradd a 1)))
        (call print (load (aptradd a 2)))
        (ret)))
