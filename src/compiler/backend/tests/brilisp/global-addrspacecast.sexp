;; CMD: guile ../../utils/sexp-json.scm < {filename} | python ../../brilisp.py | python ../../llvm.py

(brilisp
    (define ((print int) (n int)))
    (define-global (a int) (addrspace 1))
    (define ((main void))
        (set (a_cast (ptr int)) (addrspacecast a (ptr int)))
        (set (three int) (const 3))
        (store a_cast three)
        (set (a_load int) (load a))
        (set (tmp int) (call print a_load))
        (ret)))
