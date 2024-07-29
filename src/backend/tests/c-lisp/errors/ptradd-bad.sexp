(c-lisp
    (define ((print int) (n int)))

    (define ((main int))
        (declare arr (ptr int))
        (set arr (alloc int 20))
        (call print (ptradd arr 4 3))))
