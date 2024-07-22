;; ARGS: 1 2 3 4 5
(c-lisp
    (define-struct node
        (i-data int)
        (f-data float))

    (define ((print int) (n int)))
    (define ((fprint float) (n float)))

    (define ((node-print void) (n-ptr (ptr (struct node))))
        (call fprint (load (ptr-member-ref n-ptr f-data)))
        (call print (load (ptr-member-ref n-ptr i-data)))
        (ret))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare n (struct node))
        (store (member-ref n i-data) 5)
        (store (member-ref n f-data) 6.0)
        (call node-print (ptr-to n))
        (ret)))
