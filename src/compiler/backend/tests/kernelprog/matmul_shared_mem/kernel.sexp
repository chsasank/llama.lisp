(c-lisp

    (define ((print int) (n int)))

    (define ((llvm.nvvm.read.ptx.sreg.tid.x  int)) )  ;;thread id
    (define ((llvm.nvvm.read.ptx.sreg.ctaid.x int)) )   ;;block id
    (define ((llvm.nvvm.read.ptx.sreg.ctaid.y int)) )  ;; block id
    (define ((llvm.nvvm.barrier0 void)))
     
    (define-global (As (arr 1024 float)) (addrspace 3))
    (define-global (Bs (arr 1024 float)) (addrspace 3))
    
    (define  ((kernel void) 
              (M int) (N int) (K int) 
              (alpha float) (A (ptr float (addrspace 1))) 
              (B (ptr float (addrspace 1))) (beta float) 
              (C (ptr float (addrspace 1))))

        (declare cRow int)
        (declare cCol int)
    
        (set cRow (call llvm.nvvm.read.ptx.sreg.ctaid.x)) 
        (set cCol (call llvm.nvvm.read.ptx.sreg.ctaid.y)) 
        
        (declare threadCol int)
        (declare threadRow int)
        (declare BLOCKSIZE int)  
        (set BLOCKSIZE 32)  

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
                        (mul cRow BLOCKSIZE)
                         K )))

        (set B 
            (ptradd B (mul cCol BLOCKSIZE)))
            
        (set C (ptradd C 
                  (add (mul (mul cRow BLOCKSIZE) N) 
                       (mul cCol BLOCKSIZE))))

        (declare tmp float)
        (set tmp 0.0)

        (declare bkIdx int)

        (for ( (set bkIdx 0) (lt bkIdx K) (set bkIdx (add bkIdx BLOCKSIZE)))
            (store 
                (aptradd As (add (mul threadRow BLOCKSIZE) threadCol)) 
                (load (ptradd A (add (mul threadRow K) threadCol))) )

            (store 
                (aptradd Bs (add (mul  threadRow BLOCKSIZE) threadCol)) 
                (load (ptradd B (add (mul threadRow N) threadCol))) )

            (call llvm.nvvm.barrier0)  ;;__syncthreads()

            (set A (ptradd A BLOCKSIZE))
            (set B (ptradd B (mul BLOCKSIZE N)) )

            (declare dotIdx int)

            (for ( (set dotIdx 0) (lt dotIdx BLOCKSIZE) (set dotIdx (add dotIdx 1)) ) 
                (set tmp (fadd tmp 
                            (fmul (load (aptradd As (add (mul threadRow BLOCKSIZE) dotIdx)))  
                                (load (aptradd Bs (add (mul dotIdx BLOCKSIZE) threadCol)))))) 
            
            )

            (call llvm.nvvm.barrier0) 
        )

        (store (ptradd C (add (mul threadRow N) threadCol))  
                 (fadd  (fmul alpha tmp) (fmul beta  (load (ptradd C (add  (mul threadRow N) threadCol))))) )

))

