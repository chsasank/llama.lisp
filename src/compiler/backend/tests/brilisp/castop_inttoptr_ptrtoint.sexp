(brilisp
    (define ((print int) (n int)))

    (define ((ptr_print (ptr int)) (n (ptr int))))

    (define ((main void))
        (set (a int) (const -255))

        ; we will convert an integer to pointer value.
        (set (pointer_returned (ptr int)) (inttoptr a (ptr int)))
        (set (return_print_1 (ptr int)) (call ptr_print pointer_returned))

        ; convert the returned pointer value to int.
        (set (return_value int) (ptrtoint pointer_returned int))
        (set (return_print_2 int) (call print return_value))

        ; repeating the above process for a +ve int.
        (set (d int) (const 255))

        ; we will convert an integer to pointer value.
        (set (pointer_returned_1 (ptr int)) (inttoptr d (ptr int)))
        (set (return_print_3 (ptr int)) (call ptr_print pointer_returned_1))

        ; convert the returned pointer value to int.
        (set (return_value_1 int) (ptrtoint pointer_returned_1 int))
        (set (return_print_4 int) (call print return_value_1))
        (ret)))
