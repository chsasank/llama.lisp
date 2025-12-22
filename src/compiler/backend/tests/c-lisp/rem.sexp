(c-lisp
    (define ((print int) (n int)))

    (define ((main int))
        (declare i int)
        (declare j int)

        (set i 10)
        (set j 3)
        (call print (rem i j))
        (ret))

)

