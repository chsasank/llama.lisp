;; Unconditional jmp instruction.
(brilisp
    (define ((print int) (n int)))

    (define ((main void))
        (set (x int) (const 1))
        (jmp skip)
        (label unreachable)
        (set (x int) (const 999))
        (label skip)
        (set (tmp int) (call print x))
        (ret)))
