(c-lisp
  ; extern __shared__ float sdata[];
  (define-global (sdata (arr 0 float)) (addrspace 3) (linkage external))

  (define ((llvm.nvvm.read.ptx.sreg.tid.x int)))
  (define ((llvm.nvvm.barrier0 void)))

  (define ((kernel void) (n int) (out (ptr float (addrspace 1))) (inp (ptr float (addrspace 1))))
    (declare i int)
    (set i (call llvm.nvvm.read.ptx.sreg.tid.x))

    (declare s_i (ptr float))
    (set s_i (aptradd sdata i))
    (declare in_i (ptr float (addrspace 1)))
    (set in_i (ptradd inp i))
    (declare out_i (ptr float (addrspace 1)))
    (set out_i (ptradd out i))

    (store s_i (load in_i))
    (call llvm.nvvm.barrier0)
    (store out_i (load s_i))

    (ret)))
