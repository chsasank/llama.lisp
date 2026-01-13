(c-lisp
    ;  __shared__ int s[64]; Kernel to reverse an array
    (define-global (shared (arr 64 int)) (addrspace 3))
    (define ((llvm.nvvm.read.ptx.sreg.tid.x  int)))  ; threadid.x
    (define ((llvm.nvvm.barrier0 void)))              ; syncthreads()

    (define ((kernel void) (d (ptr int (addrspace 1))) (n int))
        ; int t = threadIdx.x;
        (declare t int)
        (set t (call llvm.nvvm.read.ptx.sreg.tid.x))

        ; int tr = n-t-1;
        (declare tr int)
        (set tr (sub (sub n t) 1))

        ; s[t] = d[t];
        (declare s_t (ptr int))
        (set s_t (aptradd shared t))
        (declare d_t (ptr int))
        (set d_t (ptradd d t))
        (store s_t (load d_t))

        ; __syncthreads()
        (call llvm.nvvm.barrier0)

        ; d[t] = s[tr];
        (declare s_tr (ptr int))
        (set s_tr (aptradd shared tr))
        (store d_t (load s_tr))

        (ret)))
