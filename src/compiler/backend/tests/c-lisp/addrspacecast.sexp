;; CMD: guile ../../utils/sexp-json.scm < {filename} | python ../../c-lisp.py | python ../../brilisp.py | python ../../llvm.py
(c-lisp
    (define ((main void) (b (ptr int (addrspace 1))))
        (declare tmp (ptr int (addrspace 3)))
        (set tmp (addrspacecast b (ptr int (addrspace 3))) )
        (ret)))
