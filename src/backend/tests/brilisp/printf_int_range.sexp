(brilisp
    (define ((print int) (n int)))

    (define ((int64_print int64) (n int64)))

    ;Now we shall print the range of i8
    (define ((main void))
    (set (start_i8 int8) (const -128))
    (set (end_i8 int8) (const 127))
    (set (start_i8_i32 int32) (sext start_i8 int32))
    (set (end_i8_i32 int32) (sext end_i8 int32))
    (set (temp int32) (call print start_i8_i32))
    (set (temp int32) (call print end_i8_i32))

    ;Now we shall print values outside the range of i8
    (set (start_i8 int8) (const -129))
    (set (end_i8 int8) (const 128))
    (set (start_i8_i32 int32) (sext start_i8 int32))
    (set (end_i8_i32 int32) (sext end_i8 int32))
    (set (temp int) (call print start_i8_i32))
    (set (temp int) (call print end_i8_i32))

    ;Now we shall print the range of i6
    (set (start_i16 int16) (const -32768))
    (set (end_i16 int16) (const 32767))
    (set (start_i16_i32 int32) (sext start_i16 int32))
    (set (end_i16_i32 int32) (sext end_i16 int32))
    (set (temp int32) (call print start_i16_i32))
    (set (temp int32) (call print end_i16_i32))

    ;Now we shall print values outside the range of i16
    (set (start_i16 int16) (const -32769))
    (set (end_i16 int16) (const 32768))
    (set (start_i16_i32 int32) (sext start_i16 int32))
    (set (end_i16_i32 int32) (sext end_i16 int32))
    (set (temp int32) (call print start_i16_i32))
    (set (temp int32) (call print end_i16_i32))

    ;Now we shall print the range of i32
    (set (start_i32 int32) (const -2147483648))
    (set (end_i32 int32) (const 2147483647))
    (set (temp int32) (call print start_i32))
    (set (temp int32) (call print end_i32))
    
    ;Now we shall print values outside the range of i32
    (set (start_i32 int32) (const -2147483649))
    (set (end_i32 int32) (const 2147483648))
    (set (temp int32) (call print start_i32))
    (set (temp int32) (call print end_i32))

    ;Now we shall print the range of i64
    (set (start_i64 int64) (const -9223372036854775808))
    (set (end_i64 int64) (const 9223372036854775807))
    (set (temp int64) (call int64_print start_i64))
    (set (temp int64) (call int64_print end_i64))

    ;Now we shall print values outside the range of i64 
    (set (start_i64 int64) (const -9223372036854775809))
    (set (end_i64 int64) (const 9223372036854775808))
    (set (temp int64) (call int64_print start_i64))
    (set (temp int64) (call int64_print end_i64))
    (ret)))