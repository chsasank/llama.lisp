(c-lisp
    
    (define ((llvm.memcpy.p0i8.p0i8.i64 void) (dest (ptr int8)) (src (ptr int8)) (len int64) (isvolatile bool)))
    (define-inline-brilisp ((memcpy void) (dst (ptr int8)) (src (ptr int8)) (n int))
        (set (n_64 int64) (sext n int64))
        (set (tmp void) (call llvm.memcpy.p0i8.p0i8.i64 dst src n_64 #f))
        (ret))

    (define ((llvm.nvvm.read.ptx.sreg.tid.x  int)) )  ;;thread id
    (define ((llvm.nvvm.read.ptx.sreg.ctaid.x int)) )   ;;block id
    (define ((llvm.nvvm.read.ptx.sreg.ctaid.y int)) )  ;; block id
    (define ((llvm.nvvm.barrier0 void)))
    
    (define-global (As (arr 512 float)) (addrspace 3))
    (define-global (Bs (arr 512 float)) (addrspace 3))
    (define-global (WARPSIZE int) (const 32))
    (define-global (BM int) (const 64))
    (define-global (BK int) (const 8))
    (define-global (BN int) (const 64))
    (define-global (WM int) (const 64))
    ;;(define-global (WMITER int) (const 2))
    (define-global (WN int) (const 64))
    (define-global (WNITER int) (const 2))
    ;;(define-global (WSUBN int) (const 16)) 
    (define-global (TM int) (const 8))
    (define-global (TN int) (const 4))
    (define-global (NUM_THREADS int) (const 4))

    (define-global (WSUBM int) (const (div WM WMITER)))
    (define-global (WSUBN int) (const (div WN WNITER)))
    (define-global (WMITER int) (const (div (mul WM WN) (mul (mul  (mul WARPSIZE TM) TN) WNITER))))

    (define-global (rowStrideA int) (const (div (mul NUM_THREADS 4) BK)))
    (define-global (rowStrideB int) (const (div NUM_THREADS (div BN 4))))

    
    ;;_device__

    (define  ((loadFromGmem void) 
              (N int) (K int) (A (ptr float (addrspace 1))) 
              (B (ptr float (addrspace 1))) (As (arr 512 float))
              (Bs (arr 512 float)) (innerRowA int) (innerColA int)
              (innerRowB int) (innerColB int)
              )

        (declare offset int)
        ;const float4 tmp 
        (declare tmp (arr 4 float))
         
        (for ((set offset 0) (lt (add offset rowStrideA) BM) (set offset (add offset rowStrideA)))

            ;;= reinterpret_cast<const float4 *>(
            ;;            &A[(innerRowA + offset) * K + innerColA * 4])[0];
            (declare idx int)
            (for ((set idx 0) (lt idx 4) (set idx (add idx 1)))
                (store (aptradd tmp idx) 0.0))

            (call memcpy
                    (bitcast (aptradd tmp 0) (ptr int8))
                    (bitcast (ptradd A (add (mul (add innerRowA offset) K) (mul innerColA 4)))  (ptr int8))
                    16)


            ;; As[(innerColA * 4 + 0) * BM + innerRowA + offset] = tmp.x;
            (store 
                (aptradd As (add (mul (add (mul innerColA 4) 0) BM) (add innerRowA offset))) 
                (load (aptradd tmp 0)))
            
            ;; As[(innerColA * 4 + 1) * BM + innerRowA + offset] = tmp.y;
            (store 
                (aptradd As (add (mul (add (mul innerColA 4) 1) BM) (add innerRowA offset))) 
                (load (aptradd tmp 1)))
             
            ;; As[(innerColA * 4 + 2) * BM + innerRowA + offset] = tmp.z;
            (store 
                (aptradd As (add (mul (add (mul innerColA 4) 2) BM) (add innerRowA offset))) 
                (load (aptradd tmp 2)))

            ;; As[(innerColA * 4 + 3) * BM + innerRowA + offset] = tmp.w;
            (store 
                (aptradd As (add (mul (add (mul innerColA 4) 3) BM) (add innerRowA offset))) 
                (load (aptradd tmp 3))))

        (declare first_dest (arr 4 float))
        (declare second_dest (arr 4 float))
        (for ((set offset 0) (lt (add offset rowStrideB) BK) (set offset (add offset rowStrideB)))

                ;;second_dest = reinterpret_cast<const float4 *>( &B[(innerRowB + offset) * N + innerColB * 4])[0];
                (call memcpy
                    (bitcast (aptradd second_dest 0) (ptr int8))
                    (bitcast (ptradd B (add (mul (add innerRowB offset) BN) (mul innerColB 4)) )  (ptr int8))
                    16)

                ;;first_dest = reinterpret_cast<float4 *>(&Bs[(innerRowB + offset) * BN + innerColB * 4])[0]
                (call memcpy
                    (bitcast (aptradd first_dest 0) (ptr int8))
                    (bitcast (aptradd Bs (add (mul (add innerRowB offset) BN) (mul innerColB 4)))  (ptr int8))
                    16)

                ;; first_dest = second_dest.  is it store or set? or can i do mem copy again?
                ;; (store (aptradd first_dest 0)  (load (aptradd second_dest 0)))
                ;;(set (aptradd first_dest 0)  (aptradd second_dest 0))

                (call memcpy
                    (bitcast (aptradd first_dest 0) (ptr int8))
                    (bitcast (aptradd second_dest 0)  (ptr int8))
                    16)
        )
        (ret)) 


    (define ((processFromSmem void) (regM (arr size float)) 
            (regN (arr size float)) (threadResults (ptr float))
            (As (arr 512 float)) (Bs (arr 512 float))
            (warpRow int) (warpCol int) (threadRowInWarp int) (threadColInWarp int)
            )

        (declare dotIdx int)
        (declare wSubRowIdx int)
        (declare wSubColIdx int)
        (declare i int)

        (for ((set dotIdx 0) (lt dotIdx BK) (set dotIdx (add dotIdx 1)))

            (for ((set wSubRowIdx 0) (lt wSubRowIdx WMITER) (set wSubRowIdx (add wSubRowIdx 1)))
                (for ((set i 0) (lt i TM) (set i (add i 1)))
                    (store 
                        (aptradd regM  (add (mul wSubRowIdx TM) i))
                        (load (aptradd As  (add (add (add (mul dotIdx BM) (mul warpRow WM)) (mul wSubRowIdx WSUBM)) (add (mul threadColInWarp TM) i))))
               
            )))

            (for ((set wSubColIdx 0) (lt wSubRowIdx WNITER) (set wSubColIdx (add wSubColIdx 1)))
                (for ((set i 0) (lt i TN) (set i (add i 1)))
                    (store 
                        (aptradd regN  (add (mul wSubColIdx TN) i))
                        (load (aptradd Bs  (add (add (add (mul dotIdx BN) (mul warpCol WN)) (mul wSubColIdx WSUBN)) (add (mul threadColInWarp TN) i))))
               
            )))

            (declare resIdxM int)
            (declare resIdxN int)
            (for ((set wSubRowIdx 0) (lt wSubRowIdx WMITER) (set wSubRowIdx (add wSubRowIdx 1)))
                (for ((set wSubColIdx 0) (lt wSubColIdx WNITER) (set wSubColIdx (add wSubColIdx 1)))
                    (for ((set resIdxM 0) (lt resIdxM TM) (set resIdxM (add resIdxM 1)))
                        (for ((set resIdxN 0) (lt resIdxN TN) (set resIdxN (add resIdxN 1)))

                            (store 
                                (ptradd threadResults (add (add (mul (add (mul wSubRowIdx TM) resIdxM) (mul WNITER TN)) (mul wSubColIdx TN)) resIdxN))
                                (fadd
                                    (load (ptradd threadResults (add (add (mul (add (mul wSubRowIdx TM) resIdxM) (mul WNITER TN)) (mul wSubColIdx TN)) resIdxN)))
                                    (fmul
                                        (load (aptradd regM (add (mul wSubRowIdx TM) resIdxM) )) 
                                        (load (aptradd regN (add (mul wSubColIdx TN) resIdxN))))))
                                        )))))
                                        
                                        
        (ret))


    (define  ((kernel void) 
              (M int) (N int) (K int) 
              (alpha float) (A (ptr float (addrspace 1))) 
              (B (ptr float (addrspace 1))) (beta float) 
              (C (ptr float (addrspace 1)))) 


        (declare cRow int)
        (set cRow (call llvm.nvvm.read.ptx.sreg.ctaid.y))
        (declare cCol int)
        (set cRow (call llvm.nvvm.read.ptx.sreg.ctaid.x))

        (declare warpIdx int)
        (set warpIdx (div 
                        (call llvm.nvvm.read.ptx.sreg.tid.x )
                        WARPSIZE))
        (declare warpCol int)
        (set warpCol (rem 
                        warpIdx
                        (div BN WN)))
        (declare warpRow int)
        (set warpRow (div 
                        warpIdx
                        (div BN WN)))

        (declare threadIdxInWarp int)
        (set threadIdxInWarp (rem 
                                (call llvm.nvvm.read.ptx.sreg.tid.x)
                                WARPSIZE))
        
        (declare threadColInWarp int)
        (set threadColInWarp (rem 
                                threadIdxInWarp
                                (div WSUBN TN)))

        (declare threadRowInWarp int)
        (set threadRowInWarp (div 
                                threadIdxInWarp
                                (div WSUBN TN)))

        
        (set A (ptradd A (mul (mul cRow BM) K)))
        (set B (ptradd B (mul cCol BN)))
        (set C (ptradd C (add 
                            (mul (add (mul cRow BM) (mul warpRow WM)) N)
                            (add (mul cCol BN) (mul warpCol WN)))))


        (declare innerRowA int)
        (set innerRowA (div 
                            (call llvm.nvvm.read.ptx.sreg.tid.x)
                            (div BK 4)))
        (declare innerColA int)
        (set innerColA (rem 
                            (call llvm.nvvm.read.ptx.sreg.tid.x)
                            (div BK 4)))

        (declare innerRowB int)
        (set innerRowB (div 
                          (call llvm.nvvm.read.ptx.sreg.tid.x)
                          (div BN 4)))

        (declare innerColB int)
        (set innerColB (rem 
                          (call llvm.nvvm.read.ptx.sreg.tid.x)
                          (div BN 4)))
        

        ;;should we declare theese in global with addrspace 
        (declare size int)
        (set size (mul (mul (mul WMITER TM) WNITER) TN))
        (declare threadResults (ptr float))
        (declare idx int)
        (for ((set idx 0) (lt idx size) (set idx (add idx 1)))
            (store (ptradd threadResults idx) 0.0))

        (set size (mul WMITER TM))
        (declare regM (arr size float))
        (for ((set idx 0) (lt idx size) (set idx (add idx 1)))
            (store (aptradd regM idx) 0.0))


        (set size (mul WNITER TN))
        (declare regN (arr size float))
        (for ((set idx 0) (lt idx size) (set idx (add idx 1)))
            (store (aptradd regN idx) 0.0))

        ;;;function to add in between
        (declare bkIdx int)
        (for ((set bkIdx 0) (lt bkIdx K) (set bkIdx (add bkIdx BK)))
            ;;loadFromGmem<BM, BN, BK, rowStrideA, rowStrideB>(N, K, A, B, As, Bs, innerRowA, innerColA, innerRowB, innerColB);
            (call loadFromGmem N K A B As Bs innerRowA innerColA innerRowB innerColB)
            (call llvm.nvvm.barrier0 )
            (call processFromSmem regM regN threadResults As Bs warpRow warpCol threadRowInWarp threadColInWarp)

            (set A (ptradd A BK))
            (set B (ptradd B (mul BK N)))
            (call llvm.nvvm.barrier0))

        (declare wSubRowIdx int)
        (declare wSubColIdx int)
        (declare C_interim (ptr float))
        (for ((set wSubRowIdx 0) (lt wSubRowIdx WMITER) (set wSubRowIdx (add wSubRowIdx 1)))
             (for ((set wSubColIdx 0) (lt wSubColIdx WNITER) (set wSubColIdx (add wSubColIdx 1)))
                (set C_interim (ptradd C 
                                     (add 
                                        (mul (mul wSubRowIdx WSUBM) N)
                                        (mul wSubColIdx WSUBN))))
                (declare resIdxM int)
                (for ((set resIdxM 0) (lt resIdxM TM) (set resIdxM (add resIdxM 1)))
                    (declare resIdxN int)
                    (declare tmp (arr 4 float))
                    (for ((set resIdxN 0) (lt resIdxN TN) (set resIdxN (add resIdxN 4)))

                        (set tmp 
                                (call memcpy
                                    (bitcast (aptradd tmp 0) (ptr int8))
                                    (bitcast (ptradd C_interim (add (mul (add (mul threadRowInWarp TM) resIdxM) N)  (add (mul threadColInWarp TN) resIdxN) ))  (ptr int8))
                                    16))

                        (declare i int)
                        (set i (add 
                                    (mul (add (mul wSubRowIdx TM) resIdxM) (mul WNITER TN)) 
                                    (add (mul wSubColIdx TN) resIdxN)))

                        (store (aptradd tmp 0) (fadd 
                                                    (fmul alpha (load (ptradd threadResults (add i 0))))
                                                    (fmul beta (load (aptradd tmp 0)) )))

                        (store (aptradd tmp 1) (fadd 
                                                    (fmul alpha (load (ptradd threadResults (add i 1))))
                                                    (fmul beta (load (aptradd tmp 1)) )))

                        (store (aptradd tmp 2) (fadd 
                                                    (fmul alpha (load (ptradd threadResults (add i 2))))
                                                    (fmul beta (load (aptradd tmp 2)))))

                        (store (aptradd tmp 3) (fadd 
                                                    (fmul alpha (load (ptradd threadResults (add i 3))))
                                                    (fmul beta (load (aptradd tmp 3)))))

                       
                        (declare res (arr 4 float))
                        (for ((set idx 0) (lt idx 4) (set idx (add idx 1)))
                                (store (aptradd res idx) 0.0))
                        (call memcpy
                                (bitcast (aptradd res 0) (ptr int8))
                                (bitcast (ptradd C_interim (add (mul (add (mul threadRowInWarp TM) resIdxM) N)  (add (mul threadColInWarp TN) resIdxN)))  (ptr int8))
                                16)
                        (set res tmp)
                    )
                    
                    )
                )
             )
         (ret))
            


    

 




     
        
        
)
