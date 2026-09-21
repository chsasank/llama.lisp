;; Pointer equality and ordering comparisons.
(brilisp
    (define ((print int) (n int)))

    (define ((main void))
        (set (one int) (const 1))
        (set (p (ptr int)) (alloc one))
        (set (q (ptr int)) (ptradd p 0))
        (set (r (ptr int)) (ptradd p 1))

        (set (same bool) (eq q q))
        (set (diff bool) (ne q r))
        (set (ordered bool) (lt q r))

        (br same check_diff print_fail)
        (label check_diff)
        (br diff check_ordered print_fail)
        (label check_ordered)
        (br ordered print_ok print_fail)

        (label print_fail)
        (set (tmp int) (call print 0))
        (ret)

        (label print_ok)
        (set (tmp int) (call print 1))
        (ret)))
