(c-lisp
    (define ((print int) (n int)))

    (define ((main void))
        (declare i int)

        (for ((set i 0)
              (lt i 100)
              (set i (add i 1)))
            (declare val int)
            (if (eq
                    i
                    (mul (div i 7) 7))
                (set val i)))
        (call print val)))
