;not verified as llvm is not being emitted when doing bitcast from same number of bits to same number of bits.
(brilisp
    (define ((print int32) (n int32)))

    (define ((custom_print int32) (n int8))
        (set (n_int32 int) (zext n int))
        (set (tmp int) (call print n_int32))
        (ret tmp))

    (define ((main void))
        (set (size int) (const 1))
        (set (int32_arr (ptr int)) (alloc size))
        
        (set (value int) (const 123456))
        (store int32_arr value)

        (set (int8_arr (ptr int8)) (bitcast int32_arr int8))
        
        ; int8_arr[0]
        (set (out_val int8) (load int8_arr))
        (set (tmp int) (call custom_print out_val))

        ; int8_arr[1]
        (set (array_idx int) (const 1))
        (set (int8_arr_idx (ptr int8)) (ptradd int8_arr array_idx))
        (set (out_val int8) (load int8_arr_idx))
        (set (tmp int) (call custom_print out_val))

        ; int8_arr[2]
        (set (array_idx int) (const 2))
        (set (int8_arr_idx (ptr int8)) (ptradd int8_arr array_idx))
        (set (out_val int8) (load int8_arr_idx))
        (set (tmp int) (call custom_print out_val))

        ; int8_arr[3]
        (set (array_idx int) (const 3))
        (set (int8_arr_idx (ptr int8)) (ptradd int8_arr array_idx))
        (set (out_val int8) (load int8_arr_idx))
        (set (tmp int) (call custom_print out_val))

        (ret)))
