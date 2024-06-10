(use-modules (json))

(define (list-vec-recursive sexp)
    (if (list? sexp)
        (list->vector (map list-vec-recursive sexp))
        sexp
))

(scm->json (list-vec-recursive (read)))
