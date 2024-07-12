(brilisp
    (define ((fprint float) (n float)))

    ;Function to receive a float value
    (define ((add5_float float) (n float))
        (set (five float) (const 5.2))
        (set (f_n float) (sitofp n float))
        (set (sum float) (fadd f_n five))
        (ret sum))

    ;Function to receive a int value
    (define ((add5_int float) (n int))
        (set (five float) (const 5.13))
        (set (f_n float) (sitofp n float))
        (set (sum float) (fadd f_n five))
        (ret sum))

    (define ((main void))
        ;testing for float to float conversion with a negative float value.
        (set (a float) (const -9.12))
        (set (b float) (call add5_float a))
        (set (tmp float) (call fprint b))

        ;testing for int to float conversion with a postive int
        (set (c int) (const 9))
        (set (d float) (call add5_int c))
        (set (tmp float) (call fprint d))
        (ret)))

