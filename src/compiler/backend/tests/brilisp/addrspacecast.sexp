;; CMD: guile ../../utils/sexp-json.scm < {filename} | python ../../brilisp.py | python ../../llvm.py | sed -e 's/alloca-[a-z]*/alloca/' -e 's/entry-[a-z]*/entry/'

(brilisp

    (define ((main void) (b (ptr int (addrspace 1))) ) 
        
        (set (tmp (ptr int (addrspace 3)))    (addrspacecast b (ptr (addrspace 3) ) ) )
        
        (ret)
    
    ))

        


