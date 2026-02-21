(brilisp
    (define ((fprint float) (n float)))

    (define ((main void))
        (set (v0 float) (const -1.0))
        (set (v1 float) (const 1.0))
        (set (zero float) (const 0.0))
        (set (res float) (fdiv v0 zero))
        (set (tmp float) (call fprint res))
        (set (res float) (fdiv v1 zero))
        (set (tmp float) (call fprint res))
        (set (res float) (fdiv zero zero))
        (set (tmp float) (call fprint res))
        (ret)))
