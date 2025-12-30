(brilisp

    (define ((print int) (n int)))
    
    (define-global (a int) (const 20) )
    (define-global (b int) (const 10) )

    (define ((main void)) 
        (set (tmp int) (add a b))
        (set (res int) (call print tmp))
        (ret))
)
