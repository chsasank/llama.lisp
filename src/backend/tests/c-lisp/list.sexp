;; ARGS: first 2nd 3 number_four

(c-lisp
    (define-struct Node
        (data (ptr int))
        (next (ptr (struct Node))))

    ;; TODO: use null pointers
    (define-struct List
        (len int)
        (head (ptr (struct Node))))

    (define ((print int) (n int)))
    (define ((putchar int) (c int)))
    (define ((printf int) (s (ptr int))))
    (define ((str-print void) (s (ptr int)))
        (call printf s)
        (call putchar 10)
        (ret))

    (define ((list-print void) (lst (ptr (struct List))))
        (declare (i int))
        (declare (n (ptr (struct Node))))

        (set n (load (ptr-member-ref lst head)))

        (for ((set i (load (ptr-member-ref lst len)))
              (gt i 0)
              (set i (sub i 1)))
            (call printf (load (ptr-member-ref n data)))
            (call putchar 10)
            (set n (load (ptr-member-ref n next))))
        (ret))

    (define ((list-append void) (lst (ptr (struct List))) (node (ptr (struct Node))))
        (declare (i int))
        (declare (last (ptr (struct Node))))
        (declare (len int))

        (set len (load (ptr-member-ref lst len)))
        (if (eq len 0)
            ((store (ptr-member-ref lst head) node)
             (store (ptr-member-ref lst len) (add len 1))
             (ret)))

        (set last (load (ptr-member-ref lst head)))
        (for ((set i 1)
              (lt i len)
              (set i (add i 1)))
            (set last (load (ptr-member-ref last next))))

        (store (ptr-member-ref last next) node)
        (store (ptr-member-ref lst len) (add len 1))
        (ret))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare (list (ptr (struct List))))
        (declare (node (ptr (struct Node))))

        (set list (alloc (struct List) 1))
        (store (ptr-member-ref list len) 0)

        (declare (i int))
        (for ((set i (sub argc 1))
              (gt i 0)
              (set i (sub i 1)))
            (set node (alloc (struct Node) 1))
            (store (ptr-member-ref node data) (load (ptradd argv i)))
            (call list-append list node))

        (call list-print list)
        (ret)))
