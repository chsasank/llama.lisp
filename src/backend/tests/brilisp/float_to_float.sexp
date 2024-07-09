(brilisp
    (define ((fprint float) (n float)))

    (define ((add5 float) (n float))
        (set (five float) (const 5.0))
        (set (f_n float) (sitofp n))
        (set (sum float) (fadd f_n five))
        (ret sum))

    (define ((main void))
        (set (a float) (const 9.0))
        (set (b float) (call add5 a))
        (set (tmp float) (call fprint b))
        (ret)))
