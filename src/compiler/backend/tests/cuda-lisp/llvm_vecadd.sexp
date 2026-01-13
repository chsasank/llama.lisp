;; CMD: guile ../../utils/sexp-json.scm < {filename} | python ../../cuda-lisp.py | python ../../c-lisp.py | python ../../brilisp.py | python ../../llvm.py | opt-18 -p "strip,mem2reg,simplifycfg" -S 
 
(cuda-lisp
    (define-kernel ((kernel void) (a (ptr float)) (b (ptr float)) (c (ptr float)) (n int))
        (declare x int)
        (set x (add [tid.x] (mul [bdim.x] [bid.x])))
        (if (lt x n)
            (store (ptradd c x)
                (fadd (load (ptradd a x)) (load (ptradd b x)))))))
