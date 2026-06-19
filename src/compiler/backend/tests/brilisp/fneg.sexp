;; Direct floating-point negation.
(brilisp
    (define ((fprint float) (n float)))

    (define ((main void))
        (set (a float) (const 3.5))
        (set (b float) (fneg a))
        (set (tmp float) (call fprint b))
        (set (neg2 float) (const -2.0))
        (set (c float) (fneg neg2))
        (set (tmp float) (call fprint c))
        (ret)))