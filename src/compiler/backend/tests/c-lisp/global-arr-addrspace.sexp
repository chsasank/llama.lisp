(c-lisp
    (define ((print int) (n int)))
    (define-global (a (arr 20 int)) (addrspace 3))
    (define ((main void))
        (store (aptradd a 3) 20)
        (call print (load (aptradd a 3)))
        (ret)))
