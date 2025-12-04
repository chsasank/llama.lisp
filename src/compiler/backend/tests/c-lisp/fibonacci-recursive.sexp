(c-lisp
    (define ((print int) (n int)))

    (define ((fib int) (n int))
        (if (lt n 3)
            (ret (sub n 1)))

        (ret (add
            (call fib (sub n 1))
            (call fib (sub n 2)))))

    (define ((main void))
        (call print (call fib 10))
        (ret)))
