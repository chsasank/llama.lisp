;; Explicit id instruction in brilisp.
(brilisp
    (define ((print int) (n int)))

    (define ((main void))
        (set (x int) (const 5))
        (set (y int) (id x))
        (set (tmp int) (call print y))
        (ret)))
