;not verified as llvm is not being emitted when doing bitcast from same number of bits to same number of bits.
(brilisp
    (define ((print int32) (n int32)))


    (define ((cas_t int8) (value int8))
        (set (return_value int8) (bitcast value int8))
        (ret return_value))

    (define ((main void))
        (set (value int8) (const 255))
        (set (return_value int8) (call cas_t value))
        (set(updated_to_print int32) (sext return_value int32))
        (set (tmp int32) (call print updated_to_print))
        (ret)))
