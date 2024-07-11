(brilisp
    (define ((print int8) (n int8)))

    (define ((cas_t int8) (n int32))
        (set (f_n int8) (trunc n int8))
        (ret f_n))

    (define ((main void))
        (set (a int32) (const 257))
        (set (b int8) (call cas_t a))
        (set (tmp int8) (call print b))
        (ret)))


