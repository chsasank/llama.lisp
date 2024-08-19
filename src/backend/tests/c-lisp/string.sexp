;; ARGS: Identify_Me
(c-lisp
    (declare ((puts int) (s (ptr int8))))
    (declare ((strcmp int) (s1 (ptr int8)) (s2 (ptr int8))))

    (define ((main void) (argc int) (argv (ptr (ptr int8))))
        (declare success-msg (ptr int8))
        (set success-msg "Strings are working!! 🥳")
        (if (eq 0
                (call strcmp
                    (load (ptradd argv 1))
                    "Identify_Me"))
            (call puts success-msg))))
