(c-lisp
        ; Thread Index
        (define ((llvm.nvvm.read.ptx.sreg.tid.x int)))
        ; Block Index
        (define ((llvm.nvvm.read.ptx.sreg.ctaid.x int)))
        ; Block Dimensions
        (define ((llvm.nvvm.read.ptx.sreg.ntid.x int)))

        (define ((kernel void) (a (ptr float (addrspace 1))) (b (ptr float (addrspace 1))) (c (ptr float (addrspace 1))))
                (declare x int)
                (set x
                     (add
                         (call llvm.nvvm.read.ptx.sreg.tid.x)
                         (mul (call llvm.nvvm.read.ptx.sreg.ntid.x) (call llvm.nvvm.read.ptx.sreg.ctaid.x))))

                (store (ptradd c x)
                       (fadd
                           (load (ptradd a x))
                           (load (ptradd b x))))))
