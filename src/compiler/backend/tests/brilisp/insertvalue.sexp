;; Direct insertvalue to build a struct value.
(brilisp
    (define-struct pair (int int))
    (define ((print int) (n int)))

    (define ((main void))
        (set (one int) (const 1))
        (set (p (ptr (struct pair))) (alloc one))
        (set (pv (struct pair)) (load p))
        (set (pv2 (struct pair)) (insertvalue pv 11 0))
        (set (pv3 (struct pair)) (insertvalue pv2 22 1))
        (set (v0 int) (extractvalue pv3 0))
        (set (v1 int) (extractvalue pv3 1))
        (set (tmp int) (call print v0))
        (set (tmp int) (call print v1))
        (ret)))