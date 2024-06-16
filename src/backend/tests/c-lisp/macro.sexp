(define-macro (declare-int name)
    (list 'declare (list name 'int)))

(define-macro (while cond body)
    (list
        'for
        (list 0 cond 0)
        body))

(c-lisp
    (define ((main void))
        (declare-int n)
        (set n 10))

    (while (gt n 10)
        (set n (sub n 1))))
