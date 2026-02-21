;%X = fpext float 3.125 to double ; yields double:3.125000e+00
; %Y = fpext double %X to fp128; yields fp128:0xL00000000000000004000900000000000 llvmlit for fp128 not found
(brilisp
    (define ((dprint double) (n double)))

    (define ((main void))
        (set (a float) (const 3.125))
        (set (b double) (fpext a double))
        (set (tmp double) (call dprint b))
        (ret)))
