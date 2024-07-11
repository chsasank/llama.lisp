;%Y = fptoui float 1.0E+300 to i1     ; yields undefined:1
;%Z = fptoui float 1.04E+17 to i8     ; yields undefined:1 left to implement

(brilisp
    (define ((print int32) (n int32)))

    (define ((cas_t int32) (n double))
        (set (f_n int32) (fptoui n int32))
        (ret f_n))

    (define ((main void))
        (set (a double) (const 123.0))
        (set (b int32) (call cas_t a))
        (set (tmp int32) (call print b))
        (ret )))