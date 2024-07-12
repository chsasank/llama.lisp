(brilisp
    (define ((print int) (n int)))

    (define ((ptr_print (ptr int)) (n (ptr int))))

    ;the below function converts an int value to a pointer value
    (define ((cast_inttopointer (ptr int)) (n int))
        (set (f_n (ptr int)) (inttoptr n (ptr int)))
        (ret f_n))
    
    ;the below function converts a pointer value to an int value
    (define ((cast_pointertoint int) (n (ptr int)))
        (set (return_int int) (ptrtoint n int))
        (ret return_int))

    (define ((main void))
        (set (a int) (const -255))

        ;we will convert an integer to pointer value.
        (set (pointer_returned (ptr int)) (call cast_inttopointer a))
        (set (return_print_1 (ptr int)) (call ptr_print pointer_returned ))

        ;convert the returned pointer value to int.
        (set (return_value int) (call cast_pointertoint pointer_returned))
        (set (return_print_2 int) (call print return_value))

        ;Repeating the above process for a +ve int.
        (set (d int) (const 255))

        ;we will convert an integer to pointer value.
        (set (pointer_returned_1 (ptr int)) (call cast_inttopointer d))
        (set (return_print_3 (ptr int)) (call ptr_print pointer_returned_1 ))

        ;convert the returned pointer value to int.
        (set (return_value_1 int) (call cast_pointertoint pointer_returned_1))
        (set (return_print_4 int) (call print return_value_1))
        (ret)))