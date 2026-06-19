;; Build a struct with mixed field types using insertvalue.
(c-lisp
    (define-struct mixed (i int) (f float))
    (define ((print int) (n int)))
    (define ((fprint float) (n float)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare m (struct mixed))
        (set m (insertvalue m 42 0))
        (set m (insertvalue m 3.5 1))
        (call print (extractvalue m 0))
        (call fprint (extractvalue m 1))
        (ret)))