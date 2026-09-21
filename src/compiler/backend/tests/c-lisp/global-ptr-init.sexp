;; Global pointer initialized with ptr-to another global.
(c-lisp
    (define ((print int) (n int)))

    (define-global (a int) (const 42))
    (define-global (p (ptr int)) (ptr-to a))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (load p))
        (ret)))
