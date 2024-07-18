(c-lisp
    (define ((main void))
        (declare i int)
        (set i 0)
        (for ((lt i 10) (set (add i 1))
            (call print i)))))
