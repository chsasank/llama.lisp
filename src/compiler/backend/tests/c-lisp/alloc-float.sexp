;; Alloc and use float/double arrays.
(c-lisp
    (define ((fprint float) (n float)))
    (define ((dprint double) (n double)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare farr (ptr float))
        (declare darr (ptr double))
        (declare i int)

        (set farr (alloc float 3))
        (set darr (alloc double 3))

        (for ((set i 0) (lt i 3) (set i (add i 1)))
            (store (ptradd farr i) (sitofp i float))
            (store (ptradd darr i) (sitofp i double)))

        (for ((set i 0) (lt i 3) (set i (add i 1)))
            (call fprint (load (ptradd farr i)))
            (call dprint (load (ptradd darr i))))

        (ret)))
