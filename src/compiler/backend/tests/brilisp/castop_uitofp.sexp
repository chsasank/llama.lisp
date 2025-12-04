(brilisp
    (define ((fprint float) (n float)))
    (define ((dprint double) (n double)))

    (define ((main void))
        (set (a int32) (const 257))
        (set (b float) (uitofp a float))
        (set (tmp float) (call fprint b))

        (set (a int8) (const -1))
        (set (b double) (uitofp a double))
        (set (tmp double) (call dprint b))
        (ret)))
