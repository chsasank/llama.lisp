(c-lisp
    ,@(include
        (shape.h)
        ()
        ()
        (point_t rect_t))

    (define ((print int) (n int)))

    (define ((area-of int) (rect (ptr ,rect_t)))
        (declare tl (ptr ,point_t))
        (declare br (ptr ,point_t))
        (set tl (sptradd rect top_left))
        (set br (sptradd rect bottom_right))

        (declare width int)
        (declare height int)
        (set width (sub
            (load (sptradd br x))
            (load (sptradd tl x))))
        (set height (sub
            (load (sptradd br y))
            (load (sptradd tl y))))

        (ret (mul width height)))

    (define ((main void))
        (declare tl ,point_t)
        (declare br ,point_t)
        (store (sptradd (ptr-to tl) x) 1)
        (store (sptradd (ptr-to tl) y) 2)
        (store (sptradd (ptr-to br) x) 10)
        (store (sptradd (ptr-to br) y) 5)

        (declare rect ,rect_t)
        (store (sptradd (ptr-to rect) top_left) tl)
        (store (sptradd (ptr-to rect) bottom_right) br)

        (call print (call area-of (ptr-to rect)))))
