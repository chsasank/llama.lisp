;test for sext i1 true to i32 not completed
(brilisp
    (define ((print int) (n int)))

    (define ((main void))
        (set (a int8) (const -1))
        (set (f_n int) (sext a int))
        (set (tmp int) (call print f_n))

        (set (b int8) (const 127))
        (set (f_n int) (sext b int))
        (set (tmp int) (call print f_n))

        ; test zext too
        (set (f_n int) (zext a int))
        (set (tmp int) (call print f_n))
        (ret)))
