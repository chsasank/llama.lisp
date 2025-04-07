;; ARGS: 1 2 3 4 5
(c-lisp
    (define-struct node
        (i-data int)
        (f-data float))

    (declare ((print int) (n int)))
    (declare ((fprint float) (n float)))

    (define ((node-print void) (n-ptr (ptr (struct node))))
        (call fprint (load (sptradd n-ptr f-data)))
        (call print (load (sptradd n-ptr i-data)))
        (ret))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare n (struct node))
        (store (sptradd (ptr-to n) i-data) 5)
        (store (sptradd (ptr-to n) f-data) 6.0)
        (call node-print (ptr-to n))
        (ret)))
