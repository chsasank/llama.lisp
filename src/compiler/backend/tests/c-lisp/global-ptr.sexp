(c-lisp
    (define ((print int) (n int)))
    (define-global (a int) (const 20))
    (define-global (c (ptr int)) (ptr-to a))

    (define ((main void)) 
        (declare b int)
        (set b 5)
        (declare result int)
        (set result (add b (load c)))
        (call print result)
        (ret)))