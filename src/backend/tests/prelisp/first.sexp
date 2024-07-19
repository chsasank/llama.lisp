(c-lisp
    (define ((error_check void) (res int))
        (if (gt res 0)
            (call exit 1))
        (ret res))

    (define ((main int))
        (declare res ,cudevice)
        (set res ,(myError (call print 5)))
        (ret 0)))
