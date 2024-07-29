(c-lisp
    (define ((print int) (n int)))
    (define ((fprint float) (n float)))

    (define ((iptr-set void) (p (ptr int8)))
        (store p (trunc 55 int8)))

    (define ((fptr-set void) (p (ptr double)))
        (store p (fpext 65.0 double)))

    (define ((main int))
        (declare ivar int8)
        (call iptr-set (ptr-to ivar))
        (call print (sext ivar int))

        (declare fvar double)
        (call fptr-set (ptr-to fvar))
        (call fprint (fptrunc fvar float))))
