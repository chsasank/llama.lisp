(use-modules (json))
(use-modules (ice-9 pretty-print))

(define (vec-list-recursive sexp)
    (cond 
        ((vector? sexp)
         (map vec-list-recursive (vector->list sexp)))
        ((string? sexp) (string->symbol sexp))
        (else sexp)))

(pretty-print (vec-list-recursive (json->scm)))
