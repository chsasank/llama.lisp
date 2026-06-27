;; CMD: guile ../../utils/sexp-json.scm < {filename} | python ../../c-lisp.py | python ../../brilisp.py | python ../../llvm.py
(c-lisp
    ; extern __shared__ float sdata[];
    (define-global (sdata (arr 0 float)) (addrspace 3) (linkage external))

    ; thread id and syncthreads
    (define ((llvm.nvvm.read.ptx.sreg.tid.x int)))
    (define ((llvm.nvvm.barrier0 void)))

    (define ((dynSharedCopy void) (out (ptr float (addrspace 1))) (in (ptr float (addrspace 1))) (n int))
        (declare i int)
        (set i (call llvm.nvvm.read.ptx.sreg.tid.x))

        (declare s_i (ptr float))
        (set s_i (aptradd sdata i))
        (declare in_i (ptr float (addrspace 1)))
        (set in_i (ptradd in i))
        (declare out_i (ptr float (addrspace 1)))
        (set out_i (ptradd out i))

        (store s_i (load in_i))
        (call llvm.nvvm.barrier0)
        (store out_i (load s_i))

        (ret)))
