(prelisp
    ((declare-int (lambda (name)
        `(declare (,name int))))
     (time (lambda (t)
        (car (mktime (car (strptime "%d%m%Y" t)))))))

    (c-lisp
        (define ((print int) (n int)))

        (define ((main void))
            (eval-macro declare-int val)
            (set val (eval-macro time "19062024"))
            (call print val)
            (ret))))
