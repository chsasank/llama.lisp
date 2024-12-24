(use-modules (json))

(define (list-vec-recursive sexp)
    (cond
        ((list? sexp) (list->vector (map list-vec-recursive sexp)))
        ((string? sexp) (list->vector `(string ,sexp)))
        ((keyword? sexp) (string-append "#:" (symbol->string (keyword->symbol sexp))))
        (else sexp)))

(scm->json (list-vec-recursive (read)))
