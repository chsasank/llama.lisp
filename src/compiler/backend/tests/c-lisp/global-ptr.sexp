(c-lisp
    (define ((print int) (n int)))
    (define-global (a int) (const 20))
    (define-global (c (ptr int)) (ptr-to a))
    (define-global (d int))
    (define-global (c1 (ptr int)))
    (define ((main void))
        (declare b int)
        (set b 5)
        (call print (add b (load c)))

        (set d 10)
        (call print (add b d))

        (set c1 (alloc int 10))
        (store (ptradd c1 5) -15)
        (call print (add d (load (ptradd c1 5))))

        (ret)))
