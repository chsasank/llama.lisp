(c-lisp
    (define ((print int) (n int)))

    (define ((main void))
        (declare (sum1 int))
        (set sum1 0)
        (declare (sum2 int))
        (set sum2 0)

        (declare (same-name int))

        (for ((set same-name 0)
              (lt same-name 10)
              (set same-name (add same-name 1)))
            ((declare (same-name int))
             (set same-name 0)
             (set sum2 (add sum2 same-name)))
            (set sum1 (add sum1 same-name)))

        (call print sum1)
        (call print sum2)
        (ret)))
