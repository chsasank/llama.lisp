(brilisp 
    (define ((print int) (n int)))

    (define (( main void)) 
        (set (a int) (const 127))
        (set (f_n int8) (trunc a int8))
        (set (a_n int) (sext f_n int))
        (set (tmp int) (call print a_n))
        (ret)))