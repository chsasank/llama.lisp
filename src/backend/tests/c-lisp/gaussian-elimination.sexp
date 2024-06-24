;; ARGS: 3 3  1 -1 1  3 -2 -9  2 3 -1
(c-lisp
    (define ((print int) (n int)))
    (define ((fprint float) (n float)))
    (define ((strtof float) (s (ptr int)) (end int)))
    (define ((atoi int) (s (ptr int))))

    (define ((row-op void) (mod (ptr float)) (ref (ptr float)) (len int)) ; mod -> mod - factor*ref
        (declare (i int))
        (declare (mod_p (ptr float)))
        (declare (l float))
        (declare (pv int))

        (for ((set pv 0)
              (and
                (feq (load (ptradd mod pv)) 0.0)
                (lt pv len))
              (set pv (add pv 1)))
            ())

        (if (lt pv len)
            (set l
                 (fdiv
                    (load (ptradd mod pv))
                    (load (ptradd ref pv))))
            (ret))

        (for ((set i pv)
              (lt i len)
              (set i (add i 1)))
            (store
                (set mod_p (ptradd mod i))
                (fsub
                    (load mod_p)
                    (fmul l (load (ptradd ref i))))))
            ;(call fprint (load mod_p))
        (ret))

    (define ((gaussian-eli void) (mat (ptr float)) (nrows int) (ncols int))
        (declare (ref-row int))
        (declare (mod-row int))
        (declare (col int))

        (for ((set ref-row 0)
              (lt ref-row (sub nrows 1))
              (set ref-row (add ref-row 1)))
            (for ((set mod-row (add ref-row 1))
                  (lt mod-row nrows)
                  (set mod-row (add mod-row 1)))
                (call row-op
                    (ptradd mat (mul ncols mod-row))
                    (ptradd mat (mul ncols ref-row))
                    ncols)))

        (ret))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare (input (ptr float)))
        (declare (nr int))
        (declare (nc int))
        (declare (sz int))
        (declare (i int))

        (set nr (call atoi (load (ptradd argv 1))))
        (set nc (call atoi (load (ptradd argv 2))))
        (set sz (mul nr nc))
        (set input (alloc float sz))

        (for ((set i 0)
              (lt i sz)
              (set i (add i 1)))
            (store
                (ptradd input i)
                (call strtof (load (ptradd argv (add i 3))) 0)))

        (call gaussian-eli input nr nc)
        (for ((set i 0)
              (lt i sz)
              (set i (add i 1)))
            (call fprint (load (ptradd input i))))

        (ret)))
