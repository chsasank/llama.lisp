;; Inline function returning a struct by value in brilisp.
(brilisp
    (define-struct pair (int int))
    (define ((print int) (n int)))

    (define-inline ((make-pair (struct pair)) (a int) (b int))
        (set (one int) (const 1))
        (set (p (ptr (struct pair))) (alloc one))
        (set (p0 (ptr int)) (ptradd p 0 0))
        (set (p1 (ptr int)) (ptradd p 0 1))
        (store p0 a)
        (store p1 b)
        (set (pv (struct pair)) (load p))
        (ret pv))

    (define ((main void))
        (set (pv (struct pair)) (call make-pair 7 8))
        (set (v0 int) (extractvalue pv 0))
        (set (v1 int) (extractvalue pv 1))
        (set (tmp int) (call print v0))
        (set (tmp int) (call print v1))
        (ret)))
