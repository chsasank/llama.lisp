(brilisp
    (bril-define ((print int) (n int)))

    (bril-define ((main int))
        (set (v int) (const 4))
        (jmp somewhere)
        (set (v int) (const 2))
        (label somewhere)
        (set (tmp int) (call print v))
        (ret v)))