;test for sext i1 true to i32 not completed
(brilisp
    (define ((print int16) (n int16)))

    (define ((main void))
        (set (a int8) (const -1))
        (set (f_n int16) (sext a int8))
        (set (tmp int16) (call print f_n))
        (ret )))
