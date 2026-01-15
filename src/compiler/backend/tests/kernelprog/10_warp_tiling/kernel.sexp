(c-lisp

    (define-global (WARPSIZE int) (const 32))

    (define ((llvm.nvvm.read.ptx.sreg.tid.x  int)) )  ;;thread id
    (define ((llvm.nvvm.read.ptx.sreg.ctaid.x int)) )   ;;block id
    (define ((llvm.nvvm.read.ptx.sreg.ctaid.y int)) )  ;; block id
    (define ((llvm.nvvm.barrier0 void)))

    (define-global (BM int) (const 64))
    (define-global (BK int) (const 8))
    (define-global (BN int) (const 64))
    (define-global (WM int) (const 64))
    (define-global (WMITER int) (const 2))
    (define-global (WN int) (const 64))
    (define-global (WNITER int) (const 2)
    (define-global (WSUBN int) (const 16)) 
    (define-global (TM int) (const 8))
    (define-global (TN int) (const 4))
    (define-global (NUM_THREADS int) (const 4))

    (define-global As (arr 512 float))
    (define-global Bs (arr 512 float))

    (define ((llvm.memcpy.p0i8.p0i8.i64 void) (dest (ptr int8)) (src (ptr int8)) (len int64) (isvolatile bool)))


    ;;_device__

    (define  ((loadFromGmem void) 
              (N int) (K int) (A (ptr float (addrspace 1))) 
              (B (ptr float (addrspace 1))) (As (ptr float (addrspace 3)))
              (BS (ptr float (addrspace 3))) (innerRowA int) (innerColA int)
              (innerRowB int) (innerColB int)
              )

        (declare offset int)
        ;const float4 tmp 
        (declare tmp (arr 4 float))
         

        (for ((set offset 0) (lt (add offset rowStrideA) BM) (set offset (add offset rowStrideA)))

            ;;= reinterpret_cast<const float4 *>(
            ;;            &A[(innerRowA + offset) * K + innerColA * 4])[0];
            

            (call memcpy
            (bitcast (aptradd tmp 0) (ptr float))
            (bitcast (ptradd A ((add (mul (add innerRowA offset) K) (mul innerColA 4))))  (ptr float))
            16)

            ;; As[(innerColA * 4 + 0) * BM + innerRowA + offset] = tmp.x;
            (store (aptradd As (add (mul (add (mul innerColA 4) 0) BM) 
                                    (add innerRowA offset))
                    (aptradd tmp 0)))
            
            ;; As[(innerColA * 4 + 1) * BM + innerRowA + offset] = tmp.y;
            (store (aptradd As (add (mul (add (mul innerColA 4) 1) BM) 
                                    (add innerRowA offset))
                    (aptradd tmp 1)))

            ;; As[(innerColA * 4 + 2) * BM + innerRowA + offset] = tmp.z;
            (store (aptradd As (add (mul (add (mul innerColA 4) 2) BM) 
                                    (add innerRowA offset))
                    (aptradd tmp 2)))

            ;; As[(innerColA * 4 + 3) * BM + innerRowA + offset] = tmp.w;
            (store (aptradd As (add (mul (add (mul innerColA 4) 3) BM) 
                                    (add innerRowA offset))
                    (aptradd tmp 3))))

        (declare first_dest (arr 8 float))
        (declare second_dest (arr 8 float))

        (for ((set offset 0) (lt (add offset rowStrideB) BK) (set offset (add offset rowStrideB)))

                ;;a = reinterpret_cast<float4 *>(&Bs[(innerRowB + offset) * BN + innerColB * 4])[0]
                (call memcpy
                    (bitcast (aptradd first_dest 0) (ptr float))
                    (bitcast (aptradd Bs ((add (mul (add innerRowB offset) BN) (mul innerColB 4))))  (ptr float))
                    16)

                ;;b = reinterpret_cast<const float4 *>( &B[(innerRowB + offset) * N + innerColB * 4])[0];
                (call memcpy
                    (bitcast (aptradd second_dest 0) (ptr float))
                    (bitcast (ptradd B ((add (mul (add innerRowB offset) BN) (mul innerColB 4))))  (ptr float))
                    16)

                ;; a = b 
                (store (load (aptradd first_dest 0))  (load (aptradd second_dest 0)))
        )) 

    (define ((processFromSmem void) (regM (ptr float (addrspace ..))) 
            (regN (ptr float (addrspace 1))) (threadResults (ptr float (addrspace 1)))
            (As (ptr float (addrspace 3))) (Bs (ptr float (addrspace 3)))
            (warpRow int) (warpCol int) (threadRowInWarp int) (threadColInWarp int)
            )

        (declare dotIdx int)
        (declare wSubRowIdx int)
        (declare wSubColIdx int)
        (declare i int)

        (for ((set dotIdx 0) (lt dotIdx K) (set dotIdx (add dotIdx 1)))

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

            (for ((set wSubRowIdx 0) (lt wSubRowIdx WMITER) (set wSubRowIdx (add wSubRowIdx 1)))
                (for ((set wSubColIdx 0) (lt wSubColIdx WNITER) (set wSubColIdx (add wSubColIdx 1)))
                    (for ((set resIdxM 0) (lt resIdxM TM) (set resIdxM (add resIdxM 1)))
                        (for ((set resIdxN 0) (lt resIdxN TN) (set resIdxN (add resIdxN 1)))

                            (store 
                                (ptradd threadResults (add (add (mul (add (mul wSubRowIdx TM) resIdxM) (mul WNITER TN)) (mul wSubColIdx TN)) resIdxN))
                                (add
                                    (load (ptradd threadResults (add (add (mul (add (mul wSubRowIdx TM) resIdxM) (mul WNITER TN)) (mul wSubColIdx TN)) resIdxN)))
                                    (mul
                                        (load (ptradd regM (add (mul wSubRowIdx TM) resIdxM) )) 
                                        (load (ptradd regN (add (mul wSubColIdx TN) resIdxN))))))
                                        ))))))
            


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

        ;;constexpr uint WMITER = (WM * WN) / (WARPSIZE * TM * TN * WNITER);
        (declare res int)
        (set res (div (mul WM WN)
                      (mul (mul  (mul WARPSIZE TM) TN) WNITER)))
        (set (WMITER int) (const res))

        ;;constexpr uint WSUBM = WM / WMITER
        (set res (div (mul WM WMITER)))
        (set (WSUBM int) (const res))

        ;;constexpr uint WSUBN = WN / WNITER;
        (set res (div (mul WN WNITER)))
        (set (WSUBN int) (const res))

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
        (set B (ptradd C (add 
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
        (set innerRowB (div 
                          (call llvm.nvvm.read.ptx.sreg.tid.x)
                          (div BN 4)))
        (set innerColB (rem 
                          (call llvm.nvvm.read.ptx.sreg.tid.x)
                          (div BN 4)))
        (declare rowStrideA int)
        (set rowStrideA (div 
                            (mul NUM_THREADS 4)
                            BK))
        (declare rowStrideB int)
        (set rowStrideB (div 
                            NUM_THREADS
                            (div BK 4)))

        ;;should we declare theese in global with addrspace 
        (declare size int)
        (set size (mul (mul (mul WMITER TM) WNITER) TN))
        (declare threadResults (arr size float))
        (store (aptradd threadResults 1) 0.0)

        (set size (mul WMITER TM))
        (declare regM (arr size float))
        (store (aptradd regM 1) 0.0)

        (set size (mul WNITER TN))
        (declare regN (arr size float))
        (store (aptradd regN 1) 0.0)

        ;;;function to add in between


        (declare wSubRowIdx int)
        (declare C_interim (ptr float))
        (for ((set wSubRowIdx 0) (lt wSubRowIdx WMITER) (set wSubRowIdx (add wSubRowIdx 1)))
             (for ((set wSubColIdx 0) (lt wSubColIdx WNITER) (set wSubColIdx (add wSubColIdx 1)))
                (set C_interim (ptradd C 
                                     (add 
                                        (mul (mul wSubRowIdx WSUBM) N)
                                        (mul wSubColIdx WSUBN))))
                (declare resIdxM int)
                (declare tmp (arr 4 infloat))

                (for ((set resIdxM 0) (lt resIdxM TM) (set resIdxM (add resIdxM 1)))
                    (for ((set resIdxN 0) (lt resIdxN TN) (set resIdxN (add resIdxN 4)))
                         ;;float4 tmp = reinterpret_cast<float4 *>(&C_interim[(threadRowInWarp * TM + resIdxM) * N + threadColInWarp * TN + resIdxN])[0];
                         ;;;



                         (declare i int)
                         (set i (add 
                                    (mul (add (mul wSubRowIdx TM) resIdxM) (mul WNITER TN)) 
                                    (add (mul wSubColIdx TN) resIdxN))))

                                (store (aptradd tmp 0) (add (mul alpha (load (aptradd threadResults (add i 0))
                                                            (mul beta (aptradd tmp 0))))))

                                (store (aptradd tmp 1) (add (mul alpha (load (aptradd threadResults (add i 1))
                                                            (mul beta (aptradd tmp 1))))))

                                (store (aptradd tmp 2) (add (mul alpha (load (aptradd threadResults (add i 2))
                                                            (mul beta (aptradd tmp 2))))))

                                (store (aptradd tmp 3) (add (mul alpha (load (aptradd threadResults (add i 3))
                                                            (mul beta (aptradd tmp 3))))))

                                

                    
                    
                    )
                )
             
             )
         )
    )
    
)


)