(c-lisp

    (define ((llvm.nvvm.read.ptx.sreg.tid.x  int)) )  ;;thread id
    (define ((llvm.nvvm.read.ptx.sreg.tid.y  int)) )  ;;thread id
    (define ((llvm.nvvm.read.ptx.sreg.ctaid.x int)) )   ;;block id
    (define ((llvm.nvvm.read.ptx.sreg.ctaid.y int)) )  ;; block id
    (define ((llvm.nvvm.read.ptx.sreg.ntid.x int)) )   ;;block dim
    (define ((llvm.nvvm.read.ptx.sreg.ntid.y int)) )  ;;blockdim
    (define ((llvm.nvvm.barrier0 void)))
    

    (define  ((kernel void) 
              (M int) (N int) (K int) 
              (alpha float) (A (ptr float (addrspace 1))) 
              (B (ptr float (addrspace 1))) (beta float) 
              (C (ptr float (addrspace 1))))

        (declare cRow int)
        (declare cCol int)
        ;; tile size
        (declare BLOCKSIZE int)  
        (set BLOCKSIZE 32)  

        (set cRow (call llvm.nvvm.read.ptx.sreg.ctaid.x) ) 
        (set cCol (call llvm.nvvm.read.ptx.sreg.ctaid.y) ) 

        (declare As (ptr float (addrspace 3)))   
        (declare Bs (ptr float (addrspace 3)))

        (set As (alloc float 1024))   
        (set Bs (alloc float 1024 ))
        
        (declare threadCol int)
        (declare threadRow int)

        (set threadCol 
                    (rem 
                        (call llvm.nvvm.read.ptx.sreg.tid.x) 
                        BLOCKSIZE))

        (set threadRow 
                    (div 
                        (call llvm.nvvm.read.ptx.sreg.tid.x) 
                        BLOCKSIZE))
      
        (set A 
            (ptradd A (mul 
                        K 
                        (mul cRow BLOCKSIZE) ) ) )

        (set B 
            (ptradd B (mul 
                        cCol 
                        BLOCKSIZE) ) )
            
        (set C (ptradd C 
                  (add (mul N (mul cRow BLOCKSIZE)) 
                       (mul cCol BLOCKSIZE)) ))

        (declare tmp float)
        (set tmp 0.0)

        (declare bkIdx int)

        (for ( (set bkIdx 0) (lt bkIdx K) (set bkIdx (add bkIdx BLOCKSIZE)))
            (store 
                (ptradd As (add (mul  threadRow BLOCKSIZE) threadCol)) 
                (load (ptradd A (add (mul threadRow K) threadCol))) )

            (store 
                (ptradd Bs (add (mul  threadRow BLOCKSIZE) threadCol)) 
                (load (ptradd B (add (mul threadRow N) threadCol))) )


            (call llvm.nvvm.barrier0)  ;;__syncthreads()

            (set A (ptradd A BLOCKSIZE))
            (set B (ptradd B (mul BLOCKSIZE N)) )

            (declare dotIdx int)

            (for ( (set dotIdx 0) (lt dotIdx BLOCKSIZE) (set dotIdx (add dotIdx 1)) ) 
                (set tmp (fadd tmp 
                            (fmul (load (ptradd As (add (mul threadRow BLOCKSIZE) dotIdx)))  
                                (load (ptradd Bs (add (mul dotIdx BLOCKSIZE) threadCol)))  ))) 
            
            )

            (call llvm.nvvm.barrier0) 
        )

        (store (ptradd C (add (mul threadRow N) threadCol))  
                 (fadd  (fmul alpha tmp) (fmul beta  (load (ptradd C (add  (mul threadRow N) threadCol))))) )

)

)
                






