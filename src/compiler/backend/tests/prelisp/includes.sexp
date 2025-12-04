(c-lisp
    ,@(add_print_includes)

    (define ((main int))
        (if #t
            ((call print 1)
             (call print 2)))

        (if #f
            ((call print 0))
            ((call print 3)))
             (ret 0)))
