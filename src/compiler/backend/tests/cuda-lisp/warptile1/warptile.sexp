;; CMD: guile ../../prelisp.scm < {filename}
(prelisp
    (guile
        (define BM 128)
        (define BN 128)
        (define WM 64)
        (define WN 64)
        (define BK 64)
        (define TM 8)
        (define TN 8)
        (define NUM_THREADS 256)
        (define WNITER 2)
        (define WARPSIZE 32)
        (define WMITER (/ (* WM WN) (* (*  (* WARPSIZE TM) TN) WNITER))) 
        (define WSUBM (/ WM WMITER))
        (define WSUBN (/ WN WNITER))
        (define rowStrideA (/ (* NUM_THREADS 4) BK))
        (define rowStrideB (/ NUM_THREADS (/ BN 4)))

        (define loadFromGmem (lambda (N K A B As Bs innerRowA innerColA innerRowB innerColB)
            `(  (declare offset int)
                (declare tmp (arr 4 float))
                (for ((set offset 0) (lt (add offset ,rowStrideA) ,BM) (set offset (add offset ,rowStrideA)))
                    (declare idx int)
                    (for ((set idx 0) (lt idx 4) (set idx (add idx 1)))
                        (store (aptradd tmp idx) 0.0))
                    (call memcpy4
                            (bitcast (aptradd tmp 0) (ptr int8))
                            (bitcast (ptradd ,A (add (mul (add ,innerRowA offset) K) (mul ,innerColA 4)))  (ptr int8))
                            )
                    (store 
                        (aptradd ,As (add (mul (add (mul ,innerColA 4) 0) ,BM) (add ,innerRowA offset))) 
                        (load (aptradd tmp 0)))                    
                    (store 
                        (aptradd ,As (add (mul (add (mul ,innerColA 4) 1) ,BM) (add ,innerRowA offset))) 
                        (load (aptradd tmp 1)))                    
                    (store 
                        (aptradd ,As (add (mul (add (mul ,innerColA 4) 2) ,BM) (add ,innerRowA offset))) 
                        (load (aptradd tmp 2)))
                    (store 
                        (aptradd ,As (add (mul (add (mul ,innerColA 4) 3) ,BM) (add ,innerRowA offset))) 
                        (load (aptradd tmp 3))))

                (declare first_dest (arr 4 float))
                (declare second_dest (arr 4 float))
                (for ((set offset 0) (lt (add offset ,rowStrideB) ,BK) (set offset (add offset ,rowStrideB)))
                    (call memcpy4
                        (bitcast (aptradd second_dest 0) (ptr int8))
                        (bitcast (ptradd ,B (add (mul (add ,innerRowB offset) ,BN) (mul ,innerColB 4)) )  (ptr int8))
                        )
                    (call memcpy4
                        (bitcast (aptradd first_dest 0) (ptr int8))
                        (bitcast (aptradd ,Bs (add (mul (add ,innerRowB offset) ,BN) (mul ,innerColB 4)))  (ptr int8))
                        )
                    (call memcpy4
                        (bitcast (aptradd first_dest 0) (ptr int8))
                        (bitcast (aptradd second_dest 0)  (ptr int8))
                        )
                ))))

        
        (define processFromSmem ( lambda (regM regN threadResults As Bs warpRow warpCol threadRowInWarp threadColInWarp)

            `(  (declare dotIdx int)
                (declare wSubRowIdx int)
                (declare wSubColIdx int)
                (declare i int)

                (for ((set dotIdx 0) (lt dotIdx ,BK) (set dotIdx (add dotIdx 1)))

                (for ((set wSubRowIdx 0) (lt wSubRowIdx ,WMITER) (set wSubRowIdx (add wSubRowIdx 1)))
                    (for ((set i 0) (lt i ,TM) (set i (add i 1)))
                        (store 
                            (aptradd ,regM  (add (mul wSubRowIdx ,TM) i))
                            (load (aptradd ,As  (add (add (add (mul dotIdx ,BM) (mul warpRow ,WM)) (mul wSubRowIdx ,WSUBM)) (add (mul threadColInWarp ,TM) i)))))))

                (for ((set wSubColIdx 0) (lt wSubRowIdx ,WNITER) (set wSubColIdx (add wSubColIdx 1)))
                    (for ((set i 0) (lt i ,TN) (set i (add i 1)))
                        (store 
                            (aptradd ,regN  (add (mul wSubColIdx ,TN) i))
                            (load (aptradd ,Bs (add (add (add (mul dotIdx ,BN) (mul warpCol ,WN)) (mul wSubColIdx ,WSUBN)) (add (mul threadColInWarp ,TN) i)))))))

                (declare resIdxM int)
                (declare resIdxN int)
                (for ((set wSubRowIdx 0) (lt wSubRowIdx ,WMITER) (set wSubRowIdx (add wSubRowIdx 1)))
                    (for ((set wSubColIdx 0) (lt wSubColIdx ,WNITER) (set wSubColIdx (add wSubColIdx 1)))
                        (for ((set resIdxM 0) (lt resIdxM ,TM) (set resIdxM (add resIdxM 1)))
                            (for ((set resIdxN 0) (lt resIdxN ,TN) (set resIdxN (add resIdxN 1)))
                                (store
                                    (ptradd ,threadResults (add (add (mul (add (mul wSubRowIdx ,TM) resIdxM) (mul ,WNITER ,TN)) (mul wSubColIdx ,TN)) resIdxN))
                                    (fadd
                                        (load (ptradd ,threadResults (add (add (mul (add (mul wSubRowIdx ,TM) resIdxM) (mul ,WNITER ,TN)) (mul wSubColIdx ,TN)) resIdxN)))
                                        (fmul
                                            (load (aptradd ,regM (add (mul wSubRowIdx ,TM) resIdxM) )) 
                                            (load (aptradd ,regN (add (mul wSubColIdx ,TN) resIdxN))))))
                                            ))))))))


        (define ceil-div (lambda (M N)
            `(div (add ,M (sub ,N 1)) ,N))))


    (cuda-lisp
        (define-shared As [,(* BM BK) float])
        (define-shared Bs [,(* BK BN) float])

        (define-inline-brilisp ((memcpy4 void) (dst (ptr int8)) (src (ptr int8)))
        (set (temp (struct int4))
            (asm "ld.global.v4.b32 {$0, $1, $2, $3}, [$4]; st.global.v4.b32 [$5], {$0, $1, $2, $3};"
                "=&r,=&r,=&r,=&r,l,l" src dst))
        (ret))

        (define-kernel ((kernel void) 
              (M int) (N int) (K int) 
              (alpha float) (A (ptr float)) 
              (B (ptr float)) (beta float) 
              (C (ptr float))) 

            (declare cRow int)
            (declare cCol int)
            (declare k int)
            (set cRow [tid.y])
            (set cCol [tid.x])
            (declare WARPSIZE int)
            (set WARPSIZE 32)
            

            (declare warpIdx int)
            (declare warpCol int)
            (declare warpRow int)
            (declare threadIdxInWarp int)
            (declare threadColInWarp int)
            (declare threadRowInWarp int)

            (set warpIdx (div [tid.x] WARPSIZE))
            (set warpCol (rem warpIdx (div ,BN ,WN)))
            (set warpRow (div warpIdx (div ,BN ,WN)))
            (set threadIdxInWarp (rem [tid.x] WARPSIZE))
            (set threadColInWarp (rem threadIdxInWarp (div ,WSUBN ,TN)))
            (set threadRowInWarp (div threadIdxInWarp (div ,WSUBN ,TN)))

            (set A (ptradd A (mul (mul cRow ,BM) K)))
            (set B (ptradd B (mul cCol ,BN)))
            (set C (ptradd C (add 
                            (mul (add (mul cRow ,BM) (mul warpRow ,WM)) N)
                            (add (mul cCol ,BN) (mul warpCol ,WN)))))


            (declare innerRowA int)
            (declare innerColA int)
            (declare innerRowB int)
            (declare innerColB int)

            (set innerRowA (div [tid.x] (div ,BK 4)))
            (set innerColA (rem [tid.x] (div ,BK 4)))
            (set innerRowB (div [tid.x] (div ,BN 4)))
            (set innerColB (rem [tid.x] (div ,BN 4)))

            (declare size int)
            (declare idx int)
            (declare threadResults (ptr float))
            (declare regM (arr size float))
            (declare regN (arr size float))
            (declare bkIdx int)

            (set size (mul (mul (mul ,WMITER ,TM) ,WNITER) ,TN))
            (for ((set idx 0) (lt idx size) (set idx (add idx 1)))
                  (store (ptradd threadResults idx) 0.0))

            (set size (mul ,WMITER ,TM))
            (for ((set idx 0) (lt idx size) (set idx (add idx 1)))
                (store (aptradd regM idx) 0.0))

            (set size (mul ,WNITER ,TN))
            (for ((set idx 0) (lt idx size) (set idx (add idx 1)))
                (store (aptradd regN idx) 0.0))

            (for ((set bkIdx 0) (lt bkIdx K) (set bkIdx (add bkIdx ,BK)))
                    ,(loadFromGmem 'N 'K 'A 'B 'As 'Bs 'innerRowA 'innerColA 'innerRowB 'innerColB)
                    (__syncthreads)
                    ,(processFromSmem 'regM 'regN 'threadResults 'As 'Bs 'warpRow 'warpCol 'threadRowInWarp 'threadColInWarp)
                    (set A (ptradd A ,BK))
                    (set B (ptradd B (mul ,BK N)))
                    (__syncthreads))


            (declare wSubRowIdx int)
            (declare wSubColIdx int)
            (declare C_interim (ptr float))

            (for ((set wSubRowIdx 0) (lt wSubRowIdx ,WMITER) (set wSubRowIdx (add wSubRowIdx 1)))
                (for ((set wSubColIdx 0) (lt wSubColIdx ,WNITER) (set wSubColIdx (add wSubColIdx 1)))
                    (set C_interim (ptradd C 
                                        (add 
                                            (mul (mul wSubRowIdx ,WSUBM) N)
                                            (mul wSubColIdx ,WSUBN))))
                    (declare resIdxM int)
                        (for ((set resIdxM 0) (lt resIdxM ,TM) (set resIdxM (add resIdxM 1)))
                            (declare resIdxN int)
                            (declare tmp (arr 4 float))
                                (for ((set resIdxN 0) (lt resIdxN ,TN) (set resIdxN (add resIdxN 4)))
                                    (set tmp 
                                            (call memcpy4
                                                (bitcast (aptradd tmp 0) (ptr int8))
                                                (bitcast (ptradd C_interim (add (mul (add (mul threadRowInWarp ,TM) resIdxM) N)  (add (mul threadColInWarp ,TN) resIdxN) ))  (ptr int8))
                                                ))
                                                
                                            

                                    (declare i int)
                                    (set i (add 
                                                (mul (add (mul wSubRowIdx ,TM) resIdxM) (mul ,WNITER ,TN)) 
                                                (add (mul wSubColIdx ,TN) resIdxN)))

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
                                    (call memcpy4
                                            (bitcast (aptradd res 0) (ptr int8))
                                            (bitcast (ptradd C_interim (add (mul (add (mul threadRowInWarp ,TM) resIdxM) N)  (add (mul threadColInWarp ,TN) resIdxN)))  (ptr int8))
                                            )
                                    (set res tmp)
                                ))))
            (ret))))
