(brilisp
    (bril-define ((print bool) (b bool)))

    (bril-define ((main void))
        (set (v1 int) (const 50))
        (set (v2 int) (const 51))

        (set (res bool) (eq v1 v2))
        (set (tmp bool) (call print res))

        (set (res bool) (ne v1 v2))
        (set (tmp bool) (call print res))

        (set (res bool) (ge v1 v2))
        (set (tmp bool) (call print res))

        (set (res bool) (le v1 v2))
        (set (tmp bool) (call print res))

        (set (res bool) (gt v1 v2))
        (set (tmp bool) (call print res))

        (set (res bool) (lt v1 v2))
        (set (tmp bool) (call print res))
        (ret)))
