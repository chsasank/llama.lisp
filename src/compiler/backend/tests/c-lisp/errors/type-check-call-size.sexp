(c-lisp
    (define ((print int) (n int)))

    (define ((func_print void) (n int) (m int)) 
        (call print n)
        (call print m))

    (define ((main void)) 
        (declare k int)
        (declare j int)
        (declare m int)
        (set k 50)
        (set j 20)
        (set m 100)
        (call func_print k j m)))