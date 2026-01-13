(c-lisp
    (define ((print int) (n int)))
    (define-global (b int) (const 10))
    (define ((main void))
        (declare a int)
        (set a 20)
        (call print (add a b))
        ; modify b
        (set b (sub b 5))
        (call print (mul a b))
        (ret)))
