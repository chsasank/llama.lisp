(brilisp
    (define ((print int) (n int)))

    (define ((main void))
        (set (v0 float) (const 0.0))
        (set (v1 float) (const -0.0))
        (set (res bool) (feq v0 v1))
        (br res l_true l_false)

        (label l_true)
        (set (out int) (const 1))
        (jmp out)
        (label l_false)
        (set (out int) (const 0))
        (label out)
        (set (tmp int) (call print out))
        (ret)))
