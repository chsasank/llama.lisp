(brilisp
    (bril-define ((print bool) (b bool)))

    (bril-define ((main void))
        (set (v1 float) (const 50.0))
        (set (v2 float) (const 50.1))

        (set (res bool) (feq v1 v2))
        (set (tmp bool) (call print res))

        (set (res bool) (fne v1 v2))
        (set (tmp bool) (call print res))

        (set (res bool) (fge v1 v2))
        (set (tmp bool) (call print res))

        (set (res bool) (fle v1 v2))
        (set (tmp bool) (call print res))

        (set (res bool) (fgt v1 v2))
        (set (tmp bool) (call print res))

        (set (res bool) (flt v1 v2))
        (set (tmp bool) (call print res))
        (ret)))
