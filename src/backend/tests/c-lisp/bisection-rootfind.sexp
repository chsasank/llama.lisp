(c-lisp
    (define ((fprint float) (n float)))

    (define ((quad-val float) (a2 float) (a1 float) (a0 float) (x float))
        (declare (val float))
        (set val a0)
        (set val (fadd val (fmul a1 x)))
        (set val (fadd val (fmul a2 (fmul x x))))
        (ret val))

    (define ((quad-rootfind float)
             (a2 float) (a1 float) (a0 float) ; The quadratic equation is a2(x^2) + a1(x) + a0 = 0
             (neg-pt float) (pos-pt float))   ; Known to be > 0 at pos-pt and < 0 at neg-pt

            (declare (prev-mid-val float))
            (set prev-mid-val 0.0)

            (declare (mid-val float))
            (declare (mid-pt float))
            (declare (loop bool))

            (for ((set loop #t); TODO: Use `while` and `break` once implemented
                  loop
                  0)

                (set mid-pt
                     (fdiv (fadd pos-pt neg-pt) 2.0))
                (set mid-val
                     (call quad-val a2 a1 a0 mid-pt))
                (if (feq prev-mid-val mid-val)
                    (set loop #f)
                    ((set prev-mid-val mid-val)
                     (if (fgt mid-val 0.0)
                         (set pos-pt mid-pt)
                         (set neg-pt mid-pt)))))

            (ret mid-pt))


    (define ((main void))
        (call fprint
            (call quad-rootfind
                3.0 -29.0 40.0
                5.4234 15.6352))
        (ret)))
