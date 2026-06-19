;; Function with several arguments.
(c-lisp
    (define ((print int) (n int)))

    (define ((sum5 int) (a int) (b int) (c int) (d int) (e int))
        (ret (add a (add b (add c (add d e))))))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (call sum5 1 2 3 4 5))
        (call print (call sum5 10 20 30 40 50))
        (ret)))
