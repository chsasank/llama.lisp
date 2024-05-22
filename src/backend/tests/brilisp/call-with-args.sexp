(brilisp
    ; header
    (bril-define ((print int) (n int)))
    (bril-define ((add2 int) (x int) (y int)))

    (bril-define ((main int))
        (set (x int) (const 2))
        (set (y int) (const 2))
        (set (z int) (call add2 x y))
        (set (tmp int) (call print y))
        (set (tmp int) (call print z))
        (ret tmp))

    (bril-define ((add2 int) (x int) (y int))
        (set (w int) (add x y))
        (set (y int) (const 5))
        (set (tmp int) (call print w))
        (ret w)))
