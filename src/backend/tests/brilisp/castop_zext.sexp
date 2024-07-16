;test fo zext i1 true to i32 not completed.
(brilisp
    (define ((print int) (n int)))

    (define ((main void))
        (set (a int16) (const 257))
        (set (b int) (zext a int))
        (set (tmp int) (call print b))
        
        (set (a int16) (const -1))
        (set (b int) (zext a int))
        (set (tmp int) (call print b))
        (ret)))
