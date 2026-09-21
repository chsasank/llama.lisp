;; Alloc an array of pointers and use it.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare values (arr 3 int))
        (declare ptrs (ptr (ptr int)))
        (declare i int)

        ; values = [10, 20, 30]
        (for ((set i 0) (lt i 3) (set i (add i 1)))
            (store (aptradd values i) (mul (add i 1) 10)))

        ; ptrs[i] = &values[i]
        (set ptrs (alloc (ptr int) 3))
        (for ((set i 0) (lt i 3) (set i (add i 1)))
            (store (ptradd ptrs i) (aptradd values i)))

        (for ((set i 0) (lt i 3) (set i (add i 1)))
            (call print (load (load (ptradd ptrs i)))))

        (ret)))
