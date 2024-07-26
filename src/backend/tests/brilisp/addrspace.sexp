;; CMD: guile ../../utils/sexp-json.scm < {filename} | python ../../brilisp.py | python ../../llvm.py | sed -e 's/alloca-[a-z]*/alloca/' -e 's/entry-[a-z]*/entry/'

(brilisp
    (define ((add5 void) (a (ptr float (addrspace 1))))
        (set (five float) (const 5.0))
        (store a five)
        (ret)))
