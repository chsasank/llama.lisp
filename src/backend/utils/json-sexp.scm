(use-modules (json))
(use-modules (ice-9 pretty-print))

(define (vec-list-recursive sexp)
    (cond 
        ((vector? sexp)
            (let ((op (vector-ref sexp 0)))
                (if (and
                        (eq? (vector-length sexp) 2)
                        (string? op)
                        (string=? op "string"))
                    (vector-ref sexp 1)
                    (map vec-list-recursive (vector->list sexp)))))
        ((string? sexp) (string->symbol sexp))
        (else sexp)))

(pretty-print (vec-list-recursive (json->scm)))
