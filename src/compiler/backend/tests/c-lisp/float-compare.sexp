;; Floating-point comparisons: feq, fne, flt, fgt, fle, fge.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare a float)
        (declare b float)
        (set a 2.0)
        (set b 3.0)

        (if (feq a a) (call print 1) (call print 0))
        (if (fne a b) (call print 2) (call print 0))
        (if (flt a b) (call print 3) (call print 0))
        (if (fgt b a) (call print 4) (call print 0))
        (if (fle a a) (call print 5) (call print 0))
        (if (fge b a) (call print 6) (call print 0))
        (ret)))
