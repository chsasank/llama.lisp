(brilisp
    (define ((print int) (n int)))
    (define ((fprint float) (n float)))
    (define-struct node (int float double))

    (define ((main void))
        ; arr
        (set (a (arr 10 int)) (const null))
        (set (val int) (const 10))
        ; insert creates new copy by default
        (set (a_2 (arr 10 int)) (insertvalue a val 1))
        (set (a_2_i int) (extractvalue a_2 1))
        (set (temp int) (call print a_2_i))
        ; because we don't have ssa in brilisp, you can overwrite
        (set (a (arr 10 int)) (insertvalue a val 1))
        (set (a_i int) (extractvalue a 1))
        (set (temp int) (call print a_i))

        ; struct
        (set (b (struct node)) (const null))
        (set (val_2 float) (const 5.0))
        (set (b_2 (struct node)) (insertvalue b val_2 1))
        (set (b_2_i float) (extractvalue b_2 1))
        (set (temp_2 float) (call fprint b_2_i))

        (ret)))
