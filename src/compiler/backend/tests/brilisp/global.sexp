(brilisp

    (define ((print int) (n int)))
    
    (set (a int) (const 20) )
    (set (b int) (const 10) )

    (define ((main void)) 
        (set (tmp int) (add a b))
        (set (res int) (call print tmp))
        (ret))
)
