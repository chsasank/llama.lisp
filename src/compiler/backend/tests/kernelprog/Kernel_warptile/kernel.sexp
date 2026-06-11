(c-lisp
     (define-global (As (arr 8192 float)) (addrspace 3))
     (define-global (Bs (arr 8192 float)) (addrspace 3))

     (define ((llvm.memcpy.p0i8.p0i8.i64 void) (dest (ptr int8)) (src (ptr int8)) (len int64) (isvolatile bool)))
     (define-inline-brilisp ((memcpy void) (dst (ptr int8)) (src (ptr int8)) (n int))
          (set (n_64 int64) (sext n int64))
          (set (tmp void) (call llvm.memcpy.p0i8.p0i8.i64 dst src n_64 #f))
          (ret))

     (define ((llvm.nvvm.read.ptx.sreg.tid.x int)))
     (define ((llvm.nvvm.read.ptx.sreg.ctaid.x int)))
     (define ((llvm.nvvm.read.ptx.sreg.ctaid.y int)))
     (define ((llvm.nvvm.barrier0 void)))
     (define ((kernel void) 
               (M int) (N int) (K int) (alpha float) 
               (A (ptr float (addrspace 1))) (B (ptr float (addrspace 1))) 
               (beta float) (C (ptr float (addrspace 1))))

          ;;intials
          (declare BM int)
          (set BM 128)
          (declare BN int)
          (set BN 128)
          (declare BK int)
          (set BM 16)
          (declare WM int)
          (set WM 64)
          (declare WN int)
          (set WN 64)
          (declare WNITER int)
          (set WNITER 4)
          (declare TM int)
          (set TM 8)
          (declare TN int)
          (set TN 4)
          (declare NUM_THREADS int)
          (set NUM_THREADS 128)
          (declare WARPSIZE int)
          (set WARPSIZE 32)
          
          
          (declare cRow int)
          (declare cCol int)
          (set cRow (call llvm.nvvm.read.ptx.sreg.ctaid.y))
          (set cCol (call llvm.nvvm.read.ptx.sreg.ctaid.x))

          (declare warpIdx int)
          (declare warpCol int)
          (declare warpRow int)
          (set warpIdx (div (call llvm.nvvm.read.ptx.sreg.tid.x) WARPSIZE))
          (set warpCol (rem warpIdx (div 128 64)))
          (set warpRow (div warpIdx (div 128 64)))

          (declare WMITER int)
          (declare WSUBM int)
          (declare WSUBN int)
          (set WMITER (div (mul WM WN) (mul (mul WARPSIZE TM) (mul TN WNITER))))
          (set WSUBM (div WM WMITER))
          (set WSUBN (div WN WNITER))

          (declare threadIdxInWarp int)
          (declare threadColInWarp int)
          (declare threadRowInWarp int)
          (set threadIdxInWarp (rem (call llvm.nvvm.read.ptx.sreg.tid.x) WARPSIZE))
          (set threadColInWarp (rem threadIdxInWarp (div WSUBN TN)))
          (set threadRowInWarp (div threadIdxInWarp (div WSUBN TN)))

          (set A (ptradd A (mul (mul cRow BM) K)))
          (set B (ptradd B (mul cCol BN)))
          (set C (ptradd C (add 
                              (mul (add (mul cRow BM) (mul warpRow WM)) N)
                              (add (mul cCol BN) (mul warpCol WN)))))

          (declare innerRowA int)
          (declare innerColA int)
          (declare innerRowB int)
          (declare innerColB int)
          (declare rowStrideA int)
          (declare rowStrideB int)

          (set innerRowA (div 
                             (call llvm.nvvm.read.ptx.sreg.tid.x)
                             (div BK 4)))
          (set innerColA (rem 
                             (call llvm.nvvm.read.ptx.sreg.tid.x)
                             (div BK 4)))
          (set innerRowB (div 
                             (call llvm.nvvm.read.ptx.sreg.tid.x)
                             (div BN 4)))
          (set innerColB (rem 
                             (call llvm.nvvm.read.ptx.sreg.tid.x)
                             (div BN 4)))
          (set rowStrideA (div (mul NUM_THREADS 4) BK))
          (set rowStrideA (rem (mul NUM_THREADS 4) BK))

          (declare size int)
          (set size (mul WMITER (mul TM (mul WNITER TN))))
          (declare threadResults (arr size float))
          (declare idx int)
          (for ((set idx 0) (lt idx size) (set idx (add idx 1)))
               (store (aptradd threadResults idx) 0.0))

          (set size (mul WMITER TM))
          (declare regM (arr size float))
          (for ((set idx 0) (lt idx size) (set idx (add idx 1)))
               (store (aptradd regM idx) 0.0))

          (set size (mul WNITER TN))
          (declare regN (arr size float))
          (for ((set idx 0) (lt idx size) (set idx (add idx 1)))
               (store (aptradd regN idx) 0.0))

          (declare bkIdx int)
          (declare tmp (arr 4 float))
          (for ((set bkIdx 0) (lt bkIdx K) (set bkIdx (add bkIdx BK)))
               (declare offset int)
               
               (for ((set offset 0) (le (add offset rowStrideA) BM) (set offset (add offset rowStrideA)))
                     
                    (for ((set idx 0) (lt idx 4) (set idx (add idx 1)))
                         (store (aptradd tmp idx) 0.0))
                    (call memcpy
                         (bitcast (aptradd tmp 0) (ptr int8))
                         (bitcast (ptradd A (add 
                                                (mul (add innerRowA offset) K)
                                                (mul innerColA 4))) 
                                   (ptr int8)) 16)

                    (store (aptradd As (add 
                                           (mul (add (mul innerColA 4) 0) BM)
                                           (add innerRowA offset)))
                         (load (aptradd tmp 0)))

                    (store (aptradd As (add 
                                          (mul (add (mul innerColA 4) 1) BM)
                                          (add innerRowA offset)))
                         (load (aptradd tmp 1)))

                    (store (aptradd As (add 
                                          (mul (add (mul innerColA 4) 2) BM)
                                          (add innerRowA offset)))
                         (load (aptradd tmp 2)))

                    (store (aptradd As (add 
                                          (mul (add (mul innerColA 4) 3) BM)
                                          (add innerRowA offset)))
                         (load (aptradd tmp 3))))

               (declare first_dest (arr 4 float))
               (declare second_dest (arr 4 float))
               (for ((set offset 0) (lt (add offset 8) 64) (set offset (add offset 8)))
                    (call memcpy
                         (bitcast (aptradd second_dest 0) (ptr int8))
                         (bitcast (ptradd B (add 
                                               (mul (add innerRowB offset) N)
                                               (mul innerColB 4)))
                         (ptr int8)) 16)

                    (call memcpy
                         (bitcast (aptradd first_dest 0) (ptr int8))
                         (bitcast (aptradd  Bs (add 
                                                  (mul (add innerRowB offset) BN)
                                                  (mul innerColB 4)))
                         (ptr int8)) 16)

                    (call memcpy
                         (bitcast (aptradd first_dest 0) (ptr int8))
                         (bitcast (aptradd second_dest 0) (ptr int8))
                         16))
                    
               (call llvm.nvvm.barrier0)

               (declare dotIdx int)
               (declare i int)
               (for ((set dotIdx 0) (lt dotIdx BK) (set dotIdx (add dotIdx 1)))
                    (declare wSubRowIdx int)
                    (declare wSubColIdx int)
                    (for ((set wSubRowIdx 0) (lt wSubRowIdx WMITER) (set wSubRowIdx (add wSubRowIdx 1)))
                         (for ((set i 0) (lt i TM) (set i (add i 1)))
                              (store (aptradd regM (add (mul wSubRowIdx TM) i))
                                   (load (aptradd As (add 
                                                       (add (add (mul dotIdx BM) (mul warpRow WM)) (mul wSubRowIdx WSUBM))
                                                       (add (mul threadColInWarp TM) i)))))))
                    (for ((set wSubColIdx 0) (lt wSubColIdx WNITER) (set wSubColIdx (add wSubColIdx 1)))
                         (for ((set i 0) (lt i TN) (set i (add i 1)))
                              (store (aptradd regN (add (mul wSubColIdx TN) i))
                                   (load (aptradd Bs (add 
                                                       (add (add (mul dotIdx BN) (mul warpCol WN)) (mul wSubColIdx WSUBN))
                                                       (add (mul threadColInWarp TN)  i)))))))

                    (for ((set wSubRowIdx 0) (lt wSubRowIdx WMITER) (set wSubRowIdx (add wSubRowIdx 1)))
                         (for ((set wSubColIdx 0) (lt wSubColIdx WNITER) (set wSubColIdx (add wSubColIdx 1)))
                              (declare resIdxM int)
                              (for ((set resIdxM 0) (lt resIdxM TM) (set resIdxM (add resIdxM 1)))
                                   (declare resIdxN int)
                                   (for ((set resIdxN 0) (lt resIdxN TN) (set resIdxN (add resIdxN 1)))
                                        (store (aptradd threadResults (add 
                                                                           (add 
                                                                                (mul (add (mul wSubRowIdx TM) resIdxM) (mul WNITER TN))
                                                                                (mul wSubColIdx TN))
                                                                           resIdxN))
                                             (fadd (load (aptradd threadResults (add 
                                                                                     (add (mul (add (mul wSubRowIdx 8) resIdxM) (mul WNITER TN)) 
                                                                                          (mul wSubColIdx TN))
                                                                                     resIdxN)))
                                                  (fmul (load (aptradd regM (add (mul wSubRowIdx TM) resIdxM)))
                                                        (load (aptradd regN (add (mul wSubColIdx TN) resIdxN)))))))))))

          (set A (ptradd A BK))
          (set B (ptradd B (mul BK N)))
          (call llvm.nvvm.barrier0))

          (declare C_interim (ptr float))
          (declare wSubRowIdx int)
          (declare wSubColIdx int)

          (for ((set wSubRowIdx 0) (lt wSubRowIdx WMITER) (set wSubRowIdx (add wSubRowIdx 1)))
               (for ((set wSubColIdx 0) (lt wSubColIdx WNITER) (set wSubColIdx (add wSubColIdx 1)))
                    (set C_interim (ptradd C (add 
                                                  (mul (mul wSubRowIdx WSUBM) N)
                                                  (mul wSubColIdx WSUBN))))

                    (declare resIdxM int)
                    (for ((set resIdxM 0) (lt resIdxM TM) (set resIdxM (add resIdxM 1)))
                         (declare resIdxN int)
                         (declare i int)
                         (for ((set resIdxN 0) (lt resIdxN TN) (set resIdxN (add resIdxN 4)))
                              (call memcpy
                                   (bitcast (aptradd tmp 0) (ptr int8))
                                   (bitcast (ptradd C_interim (add 
                                                                 (mul (add (mul threadRowInWarp TM) resIdxM) N)
                                                                 (add (mul threadColInWarp TN) resIdxN)))
                                        (ptr int8)) 16)
                              (set i (add 
                                        (mul (add (mul wSubRowIdx TM) resIdxM) (mul WNITER TN))
                                        (add (mul wSubColIdx TN) resIdxN)))

                              (store (aptradd tmp 0) (fadd 
                                                          (fmul alpha (load (aptradd threadResults (add i 0))))
                                                          (fmul beta (load (aptradd tmp 0)))))
                              (store (aptradd tmp 1) (fadd 
                                                         (fmul alpha (load (aptradd threadResults (add i 1))))
                                                         (fmul beta (load (aptradd tmp 1)))))
                              (store (aptradd tmp 2) (fadd 
                                                         (fmul alpha (load (aptradd threadResults (add i 2))))
                                                         (fmul beta (load (aptradd tmp 2)))))
                              (store (aptradd tmp 3) (fadd 
                                                         (fmul alpha (load (aptradd threadResults (add i 3))))
                                                         (fmul beta (load (aptradd tmp 3)))))
                              (call memcpy
                                        (bitcast (aptradd tmp 0) (ptr int8))
                                        (bitcast (ptradd C_interim (add 
                                                                      (mul (add (mul threadRowInWarp TM) resIdxM) N)
                                                                      (add (mul threadColInWarp TN) resIdxN)))
                                        (ptr int8)) 16)))))
          (ret)))
