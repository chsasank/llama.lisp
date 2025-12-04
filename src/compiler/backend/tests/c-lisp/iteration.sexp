(c-lisp
    (define ((print int) (n int)))

    (define ((main int))
        (declare i int)

        (for ((set i 11)
              (lt i 11)
              (set i (add i 1)))
             (call print i))

        (for ((set i 0)
              (lt i 10)
              (set i (add i 1)))
             (call print i))


        (ret (call print i))))
