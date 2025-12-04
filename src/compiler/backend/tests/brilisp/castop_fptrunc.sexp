;test for fptrunc double 1.0E+300 to half not done
(brilisp
    (define ((fprint float) (n float)))

    (define ((main void))
        (set (a double) (const 16777217.0))
        (set (f_n float) (fptrunc a float))
        (set (tmp float) (call fprint f_n))
        (ret)))
