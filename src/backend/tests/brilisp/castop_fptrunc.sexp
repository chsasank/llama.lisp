;test for fptrunc double 1.0E+300 to half not done
(brilisp
    (define ((fprint float) (n float)))

    (define ((cas_t float) (n double))
        (set (f_n float) (fptrunc n float))
        (ret f_n))

    (define ((main void))
        (set (a double) (const 16777217.0))
        (set (b float) (call cas_t a))
        (set (tmp float) (call fprint b))
        (ret)))


