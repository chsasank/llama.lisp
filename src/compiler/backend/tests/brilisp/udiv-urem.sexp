;; Direct unsigned division and remainder.
(brilisp
    (define ((print int) (n int)))

    (define ((main void))
        (set (a int) (const 17))
        (set (b int) (const 5))
        (set (q int) (udiv a b))
        (set (tmp int) (call print q))
        (set (r int) (urem a b))
        (set (tmp int) (call print r))

        (set (n int) (const -7))
        (set (sq int) (udiv n b))
        (set (tmp int) (call print sq))
        (set (sr int) (urem n b))
        (set (tmp int) (call print sr))
        (ret)))