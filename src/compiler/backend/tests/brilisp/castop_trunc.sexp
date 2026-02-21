(brilisp
    (define ((print int8) (n int8)))

    (define ((main void))
        (set (a int32) (const 257))
        (set (f_n int8) (trunc a int8))
        (set (tmp int8) (call print f_n))
        (ret)))
