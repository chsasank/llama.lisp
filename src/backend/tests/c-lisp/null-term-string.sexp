(c-lisp
    (define ((puts int) (s (ptr int8))))

    (define ((main void))
        (declare string (ptr int8))
        (set string (alloc int8 20))

        (declare i int)
        (for ((set i 0)
              (lt i 20)
              (set i (add i 1)))
            (store (ptradd string i) (trunc (add 97 i) int8))) ; 97 is ASCII 'a'
        (store (ptradd string 13) (trunc 0 int8))

        (call puts string)))
