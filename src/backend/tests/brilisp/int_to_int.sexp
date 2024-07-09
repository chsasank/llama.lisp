(brilisp 
    (define ((print int) (n int)))

    (define ((add5 int) (n int))
        (set (five int) (const 5))
        (set (f_n int) (fptosi n))
        (set (sum int) (add f_n five))
        (ret sum)
    )

    (define (( main void)) 
        (set (a int) (const 9))
        (set (b int) (call add5 a))
        (set (tmp int) (call print b))
        (ret)
    )


)