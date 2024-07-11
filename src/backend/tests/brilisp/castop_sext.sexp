;test for sext i1 true to i32 not completed
(brilisp
    (define ((print int16) (n int16)))

    (define ((cas_t int16) (n int8))
        (set (f_n int16) (sext n int8))
        (ret f_n))

    (define ((main void))
        (set (a int8) (const -1))
        (set (b int16) (call cas_t a))
        (set (tmp int16) (call print b))
        (ret)))

