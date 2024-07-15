;test fo zext i1 true to i32 not completed.
(brilisp
    (define ((print int64) (n int64)))

    (define ((main void))
        (set (a int32) (const 257))
        (set (b int64) (zext a int64))
        (set (tmp int64) (call print b))
        (ret)))

