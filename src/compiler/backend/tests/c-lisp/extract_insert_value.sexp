(c-lisp
    (define ((print int) (n int)))
    (define-struct int4 (x int) (y int) (z int) (w int))

    (define ((main void))
        (declare arr (arr 10 int))

        (declare i int)
        (for ((set i 0)
              (lt i 10)
              (set i (add i 1)))
            (store (aptradd arr i) i))

        (call print (extractvalue arr 5))
        (insertvalue arr 10 5)
        (call print (extractvalue arr 5))

        (declare s (struct int4))
        (insertvalue s (add 2 6) 2)
        (call print (extractvalue s 2))

        (ret)))
