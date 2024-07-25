(brilisp
    (define-string message "Strings are working!! 🥳")
    (define ((puts int) (s (ptr int8))))

    (define ((main void))
        (set (tmp (ptr int8)) (string message))
        (set (tmp int) (call puts tmp))
        (ret)))
