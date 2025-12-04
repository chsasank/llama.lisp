;%Y = fptoui float 1.0E+300 to i1     ; yields undefined:1
;%Z = fptoui float 1.04E+17 to i8     ; yields undefined:1 left to implement

(brilisp
    (define ((print int32) (n int32)))

    (define ((main void))
        (set (a double) (const 123.0))
        (set (f_n int32) (fptoui a int32))
        (set (tmp int32) (call print f_n))
        (ret)))
