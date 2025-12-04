(c-lisp
    ;; External linkage
    (define ((getchar int8)))
    (define ((exit void) (status int)))
    (define ((malloc (ptr float)) (sz int)))
    (define ((rand float)))
    ; From <math.h> (-lm)
    (define ((fabsf float) (a float)))
    (define ((fmaxf float) (a float) (b float)))
    (define ((free void) (p (ptr float))))

    ; Wrapper around `printf` from <stdio.h>
    ,printf_signature

    ; Function signatures and type aliases from Numba's CUDA driver
    ,@(get_cuda_signatures)

    (define ((ref_kernel void) (a (ptr float)) (b (ptr float)) (res (ptr float)) (N int))
        (declare i int)
        (for ((set i 0)
              (lt i N)
              (set i (add i 1)))
            (store (ptradd res i)
                   (fadd (load (ptradd a i))
                         (load (ptradd b i))))))

    (define ((error_check void) (res int) (call_str (ptr int8)))
        (if (ne res 0)
            (,(printf call_str)
             ,(printf ": Non-zero return status %d \n" res)
             (call exit res))))

    (define ((read_module void) (buf (ptr int8)))
        ,(printf "Reading kernel from standard input...\n")

        (declare c int8)
        (while (ne (set c (call getchar))
                   ,EOF)
                (store buf c)
                (set buf (ptradd buf 1)))
        (store buf (trunc 0 int8))
        (ret))


    (define ((main void))
        (declare i int)

        (declare devCount int)
        (declare device ,cu_device)
        (declare context ,cu_context)
        (declare module ,cu_module)
        (declare kernel_func ,cu_function)

        ;; CUDA initialization and context creation
        ,(error_check (call cuInit 0))
        ,(error_check (call cuDeviceGetCount (ptr-to devCount)))
        ,(error_check (call cuDeviceGet (ptr-to device) 0))
        ,(error_check (call cuCtxCreate (ptr-to context) 0 device))

        ;; Load the kernel image and get a handle to the kernel function
        (declare kernel_ptx (ptr int8))
        (set kernel_ptx (alloc int8 4000))
        (call read_module kernel_ptx)
        ,(error_check (call cuModuleLoadDataEx
            ,(void_ptr_to module) kernel_ptx
            ; num options, options, option values
            0 (inttoptr 0 (ptr int)) (inttoptr 0 (ptr (ptr int8)))))
        ,(error_check (call cuModuleGetFunction ,(void_ptr_to kernel_func) module "kernel"))

        ;; Allocate input and result
        (declare N int) (set N 32)
        (declare sz int) (set sz (mul N 4))
        (declare a (ptr float))
        (declare b (ptr float))
        (declare res_device (ptr float))
        (declare res_host (ptr float))
        (set a (call malloc sz))
        (set b (call malloc sz))
        (set res_device (call malloc sz))
        (set res_host (call malloc sz))

        (for ((set i 0)
              (lt i N)
              (set i (add i 1)))
            (store (ptradd a i) (call rand))
            (store (ptradd b i) (call rand)))

        ;; Run the reference implementation
        (call ref_kernel a b res_host N)

        ;; Copy data to device
        ; CUdeviceptr <=> (ptr int)
        (declare dev_a int64) (set dev_a (sext 0 int64))
        (declare dev_b int64) (set dev_b dev_a)
        (declare dev_res int64) (set dev_res dev_a)
        (declare sz_64 int64) (set sz_64 (sext sz int64))
        ,(error_check (call cuMemAlloc (ptr-to dev_a) sz_64))
        ,(error_check (call cuMemAlloc (ptr-to dev_b) sz_64))
        ,(error_check (call cuMemAlloc (ptr-to dev_res) sz_64))
        ,(error_check (call cuMemcpyHtoD dev_a (bitcast a ,voidptr) sz_64))
        ,(error_check (call cuMemcpyHtoD dev_b (bitcast b ,voidptr) sz_64))

        ;; Launch the kernel and wait
        ; Array of CUdeviceptr *
        (declare KernelParams (ptr (ptr int64)))
        (set KernelParams (alloc (ptr int64) 3))
        (store KernelParams (ptr-to dev_a))
        (store (ptradd KernelParams 1) (ptr-to dev_b))
        (store (ptradd KernelParams 2) (ptr-to dev_res))
        (declare BlockSize int) (set BlockSize 32)
        (declare GridSize int)
        (set GridSize (div (sub (add N BlockSize) 1)
                            BlockSize))
        ,(error_check (call cuLaunchKernel kernel_func
                             ; Grid sizes X, Y, Z
                             GridSize 1 1
                             ; Block sizes X, Y, Z
                             BlockSize 1 1
                             ; Shared mem size, stream id, kernel params, extra options
                             0 ,nullptr (bitcast KernelParams (ptr (ptr int8))) (bitcast ,nullptr (ptr (ptr int8)))))
        ,(error_check (call cuCtxSynchronize))

        ;; Retieve and verify results
        ,(error_check (call cuMemcpyDtoH (bitcast res_device ,voidptr) dev_res sz_64))
        (declare max_err float) (set max_err 0.0)
        (for ((set i 0)
              (lt i N)
              (set i (add i 1)))
            (declare diff float)
            (set diff (fsub (load (ptradd res_host i))
                            (load (ptradd res_device i))))
            (set max_err
                 (call fmaxf max_err (call fabsf diff))))
        ,(printf "Max error: %f\n" max_err)

        ;; Cleanup
        (call free a)
        (call free b)
        (call free res_host)
        (call free res_device)
        ,(error_check (call cuMemFree dev_a))
        ,(error_check (call cuMemFree dev_b))
        ,(error_check (call cuMemFree dev_res))
        ,(error_check (call cuModuleUnload module))
        ,(error_check (call cuCtxDestroy context))
        (ret)))
