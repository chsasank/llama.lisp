(brilisp
    (define ((fprint float) (n float)))
    (define ((print int) (n int)))

    (define-struct
        node
        (int float))

    (define ((main void))
        (set (i-val int) (const 3))
        (set (f-val float) (const 4.0))
        (set (one int) (const 1))
        (set (n (ptr (struct node))) (alloc one))

        (set (elem (ptr int)) (ptradd n 0 0))
        (store elem i-val)

        (set (elem (ptr float)) (ptradd n 0 1))
        (store elem f-val)

        (set (elem (ptr float)) (ptradd n 0 1))
        (set (tmp float) (load elem))
        (set (tmp float) (call fprint tmp))

        (set (elem (ptr int)) (ptradd n 0 0))
        (set (tmp int) (load elem))
        (set (tmp int) (call print tmp))
        (ret)))
