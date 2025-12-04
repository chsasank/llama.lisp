(brilisp
    (define ((print bool) (b bool)))

    (define ((main void))
        (set (zero float) (const 0.0))
        (set (nan float) (fdiv zero zero))
        (set (res bool) (feq nan nan))
        (set (res bool) (not res))
        (set (tmp bool) (call print res))
        (ret)))
