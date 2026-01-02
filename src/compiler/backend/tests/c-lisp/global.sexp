(c-lisp

    (define ((print int) (n int)))
    
    (define-global (a int) (const 20) )
    (define-global (b int) (const 10) )

    (define ((main void)) 

        (declare tmp int)
        (set tmp (add a b))

        (call print tmp)
        (ret))
)
