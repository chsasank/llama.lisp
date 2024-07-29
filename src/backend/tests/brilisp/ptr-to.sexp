(brilisp
    (define ((print int) (n int)))

    (define ((ptr-set void) (p (ptr int)) (n int))
        (store p n)
        (ret))

    (define ((main void))
        (set (num int) (const 5))
        (set (tmp void) (call print num))

        (set (val int) (const 10))
        (set (num-ptr (ptr int)) (ptr-to num))
        (set (tmp void) (call ptr-set num-ptr val))
        (set (tmp void) (call print num))
        (ret)))
