;; Mutually recursive even/odd functions using forward declarations.
(c-lisp
    (define ((print int) (n int)))

    (define ((even bool) (n int)))

    (define ((odd bool) (n int))
        (if (eq n 0)
            (ret #f)
            (ret (call even (sub n 1)))))

    (define ((even bool) (n int))
        (if (eq n 0)
            (ret #t)
            (ret (call odd (sub n 1)))))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (if (call even 4) (call print 1) (call print 0))
        (if (call odd 3) (call print 2) (call print 0))
        (if (call even 5) (call print 0) (call print 3))
        (if (call odd 0) (call print 0) (call print 4))
        (ret)))
