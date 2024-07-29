(c-lisp
    (define ((print int) (n int)))
    (define ((fprint float) (n float)))

    (define ((print-2 void) (n int))
        (call print (load (ptr-to n))))

    (define ((main int))
        (declare var int)
        (set var 10)
        (call print-2 var)))
