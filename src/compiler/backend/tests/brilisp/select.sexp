;; Direct select between integers and floats.
(brilisp
    (define ((print int) (n int)))
    (define ((fprint float) (n float)))

    (define ((main void))
        (set (t bool) (const #t))
        (set (f bool) (const #f))
        (set (a int) (const 1))
        (set (b int) (const 2))
        (set (x int) (select t a b))
        (set (tmp int) (call print x))
        (set (y int) (select f a b))
        (set (tmp int) (call print y))

        (set (fa float) (const 1.5))
        (set (fb float) (const 2.5))
        (set (fx float) (select t fa fb))
        (set (tmp float) (call fprint fx))
        (set (fy float) (select f fa fb))
        (set (tmp float) (call fprint fy))
        (ret)))