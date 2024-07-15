(brilisp
    (define ((fprint float) (n float)))

    (define ((dprint double) (n double)))

    (define ((main void))
        (set (a int32) (const 257))
        (set (b float) (uitofp a float))
        (set (tmp float) (call fprint b))
        (set (ui_8 int8) (const -1))
        (set (c double) (uitofp ui_8 double))
        (set (tmp double) (call dprint c))
        (ret)))