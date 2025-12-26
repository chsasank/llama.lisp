(c-lisp

    (define ((main void) (b (ptr int (addrspace 1)))) 

        (declare tmp (ptr int (addrspace 3)) )
        (set tmp (addrspacecast b (ptr int (addrspace 3))) )
        (ret)
    
    )
)
