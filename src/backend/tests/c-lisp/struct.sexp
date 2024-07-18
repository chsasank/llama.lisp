;; ARGS: 1 2 3 4 5
(c-lisp
    (define-struct node
        (i-data int)
        (f-data float))

    (define ((print int) (n int)))
    (define ((fprint float) (n float)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare n (ptr (struct node)))
        (set n (alloc (struct node) 1))

        (store (ptr-member-ref n i-data) 5)
        (store (ptr-member-ref n f-data) 6.0)

        (call fprint (load (ptr-member-ref n f-data)))
        (call print (load (ptr-member-ref n i-data)))
        (ret)))
