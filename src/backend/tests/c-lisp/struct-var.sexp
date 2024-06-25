(c-lisp
    (define-struct Node
        (i int)
        (f float))

    (define ((print int) (n int)))
    (define ((fprint float) (n float)))

    (define ((node-print void) (node (struct Node)))
        (call print (load (struct-var-index node i)))
        (call fprint (load (struct-var-index node f)))
        (ret))

    (define ((struct-init void) (node (ptr (struct Node))))
        (declare (val (struct Node)))
        (store (struct-var-index val i) 4)
        (store (struct-var-index val f) 5.0)

        (store node val)
        (ret))

    (define ((main void))
        (declare (n-ptr (ptr (struct Node))))
        (set n-ptr (alloc (struct Node) 1))
        (call struct-init n-ptr)

        (declare (n-var (struct Node)))
        (set n-var (load n-ptr))
        (call node-print n-var)
        (ret)))
