(brilisp
    (define ((fprint float) (n float)))

    (define ((dprint double) (n double)))

    (define ((cas_t float) (n int32))
        (set (f_n float) (uitofp n float))
        (ret f_n))
    
    (define ((cas_t_diff_variable double) (n int8))
        (set (f_n double) (uitofp n double))
        (ret f_n))

    (define ((main void))
        (set (a int32) (const 257))
        (set (b float) (call cas_t a))
        (set (tmp float) (call fprint b))
        (set (ui_8 int8) (const -1))
        (set (c double) (call cas_t_diff_variable ui_8))
        (set (tmp double) (call dprint c))

        (ret )))