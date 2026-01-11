;; CMD: guile ../../utils/sexp-json.scm < {filename} | python ../../cuda-lisp.py | guile ../../utils/json-sexp.scm

(cuda-lisp
    (define-kernel ((kernel void) (a (ptr float)) (b (ptr float)) (c (ptr float)) (len int))
        ; Calculate c[i, j] from a[i, *] and b[*, j]
        ; a, b, c have dimensions len x len
        (declare row int)
        (declare col int)
        (set row (add [tid.x] (mul [bdim.x] [bid.x])))
        (set col (add [tid.y] (mul [bdim.y] [bid.y])))

        (declare-global a-ptr (ptr float))
        (declare-global b-ptr (ptr float))
        (declare-global c-ptr (ptr float))
        (set a-ptr (ptradd a (mul row len)))
        (set b-ptr (ptradd b col))
        (set c-ptr (ptradd c (add col (mul row len))))

        (declare c-val float)
        (set c-val 0.0)

        (declare k int)
        (for ((set k 0)
            (lt k len)
            (set k (add k 1)))
            (set c-val
                (fadd c-val (fmul (load a-ptr) (load b-ptr))))
            (set a-ptr (ptradd a-ptr 1))
            (set b-ptr (ptradd b-ptr len)))

        (store c-ptr c-val)))