;; CMD: guile ../../utils/sexp-json.scm < {filename} | guile ../../utils/json-sexp.scm
(brilisp
    (define ((puts int) (s (ptr int8))))

    (define ((main void))
        (set (tmp (ptr int8)) (string-ref message))
        (set (tmp int) (call puts tmp))
        (ret))

    (define-string message "Strings are working!! 🥳"))
