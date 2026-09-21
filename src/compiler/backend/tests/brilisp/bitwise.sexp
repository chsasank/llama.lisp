;; Direct bitwise operations: shl, lshr, ashr, xor.
(brilisp
    (define ((print int) (n int)))

    (define ((main void))
        (set (a int) (const 1))
        (set (b int) (const 4))
        (set (s int) (shl a b))
        (set (tmp int) (call print s))

        (set (c int) (const 255))
        (set (d int) (const 3))
        (set (lr int) (lshr c d))
        (set (tmp int) (call print lr))

        (set (e int) (const -32))
        (set (ar int) (ashr e d))
        (set (tmp int) (call print ar))

        (set (x int) (const 85))
        (set (y int) (const 170))
        (set (xored int) (xor x y))
        (set (tmp int) (call print xored))
        (ret)))