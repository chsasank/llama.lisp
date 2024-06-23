;; ARGS: 1 2 3 4 5
(c-lisp
    (define-struct node
        (i-data int)
        (f-data float))

    (define ((print int) (n int)))
    (define ((fprint float) (n float)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare (n (ptr (struct node))))
        (set n (alloc (struct node) 1))

        (store (struct-memb n i-data) 5)
        (store (struct-memb n f-data) 6.0)

        (call fprint (load (struct-memb n f-data)))
        (call print (load (struct-memb n i-data)))
        (ret)))
