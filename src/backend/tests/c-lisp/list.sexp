;; ARGS: first 2nd 3 number_four

(c-lisp
    (define-struct Node
        (data (ptr int))
        (next (ptr (struct Node))))

    ;; TODO: use null pointers
    (define-struct List
        (len int)
        (head (ptr (struct Node))))

    (declare ((print int) (n int)))
    (declare ((putchar int) (c int)))
    (declare ((printf int) (s (ptr int))))
    (define ((str-print void) (s (ptr int)))
        (call printf s)
        (call putchar 10)
        (ret))

    (define ((list-print void) (lst (ptr (struct List))))
        (declare i int)
        (declare n (ptr (struct Node)))

        (set n (load (sptradd lst head)))

        (for ((set i (load (sptradd lst len)))
              (gt i 0)
              (set i (sub i 1)))
            (call printf (load (sptradd n data)))
            (call putchar 10)
            (set n (load (sptradd n next))))
        (ret))

    (define ((list-append void) (lst (ptr (struct List))) (node (ptr (struct Node))))
        (declare i int)
        (declare last (ptr (struct Node)))
        (declare len int)

        (set len (load (sptradd lst len)))
        (if (eq len 0)
            ((store (sptradd lst head) node)
             (store (sptradd lst len) (add len 1))
             (ret)))

        (set last (load (sptradd lst head)))
        (for ((set i 1)
              (lt i len)
              (set i (add i 1)))
            (set last (load (sptradd last next))))

        (store (sptradd last next) node)
        (store (sptradd lst len) (add len 1))
        (ret))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare list (ptr (struct List)))
        (declare node (ptr (struct Node)))

        (set list (alloc (struct List) 1))
        (store (sptradd list len) 0)

        (declare i int)
        (for ((set i (sub argc 1))
              (gt i 0)
              (set i (sub i 1)))
            (set node (alloc (struct Node) 1))
            (store (sptradd node data) (load (ptradd argv i)))
            (call list-append list node))

        (call list-print list)
        (ret)))
