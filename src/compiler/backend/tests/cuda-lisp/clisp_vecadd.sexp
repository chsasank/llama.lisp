;; CMD: guile ../../utils/sexp-json.scm < {filename} | python ../../cuda-lisp.py | guile ../../utils/json-sexp.scm

(cuda-lisp
    (define-kernel ((kernel void) (a (ptr float)) (b (ptr float)) (c (ptr float)))
        (declare x int)
        (set x (add [tid.x] (mul [bdim.x] [bid.x])))
        (store (ptradd c x)
            (fadd
                (load (ptradd a x))
                (load (ptradd b x))))))
