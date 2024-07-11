(brilisp
    (define ((print int) (n int)))

    (define ((cast_inttopointer (ptr int)) (n int))
        (set (f_n (ptr int))(inttoptr n (ptr int)))
        (ret f_n))
    
    (define ((cast_pointertoint int) (n (ptr int)))
        (set (return_int int) (ptrtoint n int))
        (ret return_int))

    (define ((main void))
        (set (a int) (const 255))
        (set (pointer_returned (ptr int)) (call cast_inttopointer a))
        (set (return_value int) (call cast_pointertoint pointer_returned))
        (set (return_print int) (call print return_value))
        (ret )))