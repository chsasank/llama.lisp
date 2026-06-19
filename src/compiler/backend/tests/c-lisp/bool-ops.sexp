;; Boolean operators: and, or, not.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare a bool)
        (declare b bool)
        (set a #t)
        (set b #f)

        ; and
        (if (and a (not b))
            (call print 1)
            (call print 0))

        ; or
        (if (or b a)
            (call print 2)
            (call print 0))

        ; not
        (if (not (and a b))
            (call print 3)
            (call print 0))

        (ret)))
