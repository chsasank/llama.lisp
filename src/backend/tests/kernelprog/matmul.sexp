(c-lisp
        ; Thread Index
        (define ((llvm.nvvm.read.ptx.sreg.tid.x int)))
        (define ((llvm.nvvm.read.ptx.sreg.tid.y int)))
        ; Block Index
        (define ((llvm.nvvm.read.ptx.sreg.ctaid.x int)))
        (define ((llvm.nvvm.read.ptx.sreg.ctaid.y int)))
        ; Block Dimensions
        (define ((llvm.nvvm.read.ptx.sreg.ntid.x int)))
        (define ((llvm.nvvm.read.ptx.sreg.ntid.y int)))

        (define ((kernel void) (a (ptr float)) (b (ptr float)) (c (ptr float)) (len int))
                ; Calculate c[i, j] from a[i, *] and b[*, j]
                ; a, b, c have dimensions len x len
                (declare row int)
                (declare col int)
                (set row
                     (add
                        (call llvm.nvvm.read.ptx.sreg.tid.x)
                        (mul (call llvm.nvvm.read.ptx.sreg.ntid.x) (call llvm.nvvm.read.ptx.sreg.ctaid.x))))
                (set col
                     (add
                        (call llvm.nvvm.read.ptx.sreg.tid.y)
                        (mul (call llvm.nvvm.read.ptx.sreg.ntid.y) (call llvm.nvvm.read.ptx.sreg.ctaid.y))))

                (declare a-ptr (ptr float))
                (declare b-ptr (ptr float))
                (declare c-ptr (ptr float))
                (set a-ptr (ptradd a (mul row len)))
                (set b-ptr (ptradd b col))
                (set c-ptr (ptradd c (add col (mul row len))))

                (declare c-val float)
                (set c-val 0.0)

                (declare k int)
                (for ((set k 0)
                      (lt k len)
                      (set k (add k 1)))
                     (set c-val
                          (fadd
                             c-val
                             (fmul (load a-ptr) (load b-ptr))))
                     (set a-ptr (ptradd a-ptr 1))
                     (set b-ptr (ptradd b-ptr len)))

                (store c-ptr c-val)))