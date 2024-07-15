(brilisp
    (define ((fprint float) (n float)))


    (define ((main void))
        ;testing for float to float conversion with a negative float value.
        (set (a float) (const -9.12))
        (set (five float) (const 5.2))
        (set (f_n float) (sitofp a float))
        (set (sum float) (fadd f_n five))
        (set (tmp float) (call fprint sum))

        ;testing for int to float conversion with a postive int
        (set (c int) (const 9))
        (set (five float) (const 5.13))
        (set (f_n float) (sitofp c float))
        (set (sum float) (fadd f_n five))
        (set (tmp float) (call fprint sum))
        (ret)))

