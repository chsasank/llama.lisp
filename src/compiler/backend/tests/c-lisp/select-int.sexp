;; Select between integers based on a boolean condition.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (call print (select #t 1 2))
        (call print (select #f 1 2))
        (call print (select (lt 1 2) 10 20))
        (call print (select (gt 1 2) 10 20))
        (call print (select (eq 3 3) 100 200))
        (ret)))