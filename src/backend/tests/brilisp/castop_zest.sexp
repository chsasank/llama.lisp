;test fo zext i1 true to i32 not completed.
(brilisp
    (define ((print int64) (n int64)))

    (define ((cas_t int64) (n int32))
        (set (f_n int64) (zext n int64))
        (ret f_n))

    (define ((main void))
        (set (a int32) (const 257))
        (set (b int64) (call cas_t a))
        (set (tmp int64) (call print b))
        (ret )))

