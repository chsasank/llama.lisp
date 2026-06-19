;; Direct extractvalue from a struct value.
(brilisp
    (define-struct pair (int int))
    (define ((print int) (n int)))

    (define ((main void))
        (set (one int) (const 1))
        (set (p (ptr (struct pair))) (alloc one))
        (set (p0 (ptr int)) (ptradd p 0 0))
        (set (p1 (ptr int)) (ptradd p 0 1))
        (store p0 10)
        (store p1 20)
        (set (pv (struct pair)) (load p))
        (set (v0 int) (extractvalue pv 0))
        (set (v1 int) (extractvalue pv 1))
        (set (tmp int) (call print v0))
        (set (tmp int) (call print v1))
        (ret)))
