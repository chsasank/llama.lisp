(c-lisp
    (define ((print int) (n int)))

    (define ((main int))
        (declare (i int))
        (for (set i 0)
             (< i 10)
             (set i (+ i 1))
             (call print i))

        (ret (call print i))))
