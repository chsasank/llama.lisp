;; Regression test for arrays whose element type is a struct.
;; brilisp.py used to crash in gen_type because it treated typ[2]
;; (the element type) as a potential addrspace token.
(brilisp
    (define ((print int) (n int)))
    (define-struct node (int))

    (define-global (arr (arr 3 (struct node))))

    (define ((main void))
        ; arr[1].val = 42
        (set (one int) (const 1))
        (set (elem (ptr (struct node))) (ptradd arr 0 one))
        (set (field (ptr int)) (ptradd elem 0 0))
        (set (forty-two int) (const 42))
        (store field forty-two)

        ; print arr[1].val
        (set (loaded int) (load field))
        (set (tmp int) (call print loaded))
        (ret)))
