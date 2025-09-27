(c-lisp
    (define ((main void))
        (declare arr1 (ptr int))
        ,@(arr-init arr (1 2 3) 5)

        (declare arr2 (ptr int))
        ,@(arr-init arr2 () 5)))
