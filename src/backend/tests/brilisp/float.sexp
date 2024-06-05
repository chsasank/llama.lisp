(brilisp
    (bril-define ((fprint float) (n float)))

    (bril-define ((main void))
        (set (v0 float) (const 9.0))
        (set (v1 float) (const -20.0))
        (set (res float) (fdiv v0 v1))
        (set (tmp float) (call fprint res))

        (set (v2 float) (const .1))
        (set (v3 float) (const 0.1))
        (set (add1 float) (fadd v2 v3))
        (set (dres float) (fadd add1 v2))
        (set (tmp float) (call fprint dres))

        (set (v4 float) (const .1))
        (set (add2 float) (fadd v4 v4))
        (set (fres float) (fadd add2 v4))
        (set (tmp float) (call fprint fres))
        (ret)))
