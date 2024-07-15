;not verified as llvm is not being emitted when doing bitcast from same number of bits to same number of bits.
(brilisp
    (define ((print int32) (n int32)))

    (define ((main void))
        (set (value int8) (const 255))
        (set (casted_value int8) (bitcast value int8))
        (set (updated_to_print int32) (sext casted_value int32))
        (set (tmp int32) (call print updated_to_print))
        (ret)))
