(c-lisp
    (define ((print int) (n int)))

    (define ((func_print void) (n int) (m float)) 
        (call print n)
        (call print m))

    (define ((main void)) 
        (declare k int)
        (declare j float)
        (set k 50)
        (set j 10.0)
        (call func_print k j)))