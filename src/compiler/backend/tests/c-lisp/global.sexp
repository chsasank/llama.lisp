(c-lisp
    (define ((print int) (n int)))
    ;;(define-global (a int) (const 20))
    (define-global (b int) (const 10))

    (define ((main void)) 
        (declare a int)
        (set a 20)
        (call print (add a b))
        (declare tmp int)
        (set tmp (add a b))
        (call print tmp)
        ; modify b
        (set b (sub b 5))
        (call print (mul a b))
        (ret)))