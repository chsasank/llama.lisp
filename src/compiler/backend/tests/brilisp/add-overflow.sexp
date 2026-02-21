(brilisp
    (define ((print int) (n int)))

    (define ((pow int) (base int) (ex int))
        (set (out int) (const 1))
        (set (one int) (const 1))

        (label loop)
        (set (end bool) (lt ex one))
        (br end ret body)

        (label body)
        (set (out int) (mul out base))
        (set (ex int) (sub ex one))
        (jmp loop)

        (label ret)
        (ret out))

    (define ((main int))
        (set (one int) (const 1))
        (set (two int) (const 2))
        (set (thiry int) (const 30))

        (set (half int) (call pow two thiry))
        (set (tmp int) (call print half))

        (set (smaller int) (sub half one))
        (set (maxint int) (add half smaller))
        (set (tmp int) (call print maxint))

        (set (overflow int) (add maxint one))
        (set (tmp int) (call print overflow))
        (ret tmp)))
