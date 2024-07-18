(c-lisp
    ;; Constants that we can't currently define in C-Lisp
    ;; TODO [macro] [string]
    (define ((reading_stdin_msg (ptr int8))))
    (define ((eof_char int8)))
    (define ((null_char int8)))
    (define ((kernel_name_str (ptr int8))))
    (define ((error_status_msg (ptr int8))))

    ;; External linkage
    (define ((puts int) (s (ptr int8))))
    (define ((print void) (n int)))
    (define ((getchar int8)))
    (define ((exit void) (status int)))
    ;; TODO [macro]: enums/symbolic constants
    (define ((cuInit int) (flags int)))
    (define ((cuDeviceGetCount int) (n (ptr int))))
    (define ((cuDeviceGet int) (ptr (ptr int)) (ordinal int)))
    (define ((cuCtxCreate int) (context (ptr (ptr int))) (flags int) (device int)))
    (define ((cuModuleLoadData int) (module (ptr (ptr int))) (data (ptr int8))))
    (define ((cuModuleGetFunction int) (func (ptr (ptr int))) (module (ptr int)) (name (ptr int8))))
    (define ((cuMemAlloc int) (pntr (ptr (ptr int))) (n int)))
    (define ((cuMemFree int) (pntr (ptr int))))

    (define ((ref_kernel void) (a (ptr float)) (b (ptr float)) (res (ptr float)) (N int))
        (declare i int)
        (for ((set i 0)
              (lt i N)
              (set i (add i 1)))
            (store (ptradd res i)
                   (fadd (load (ptradd a i))
                         (load (ptradd b i))))))

    ;; TODO [macro] [string]: macro the call to this function
    (define ((error_check void) (res int) (call_str (ptr int8)))
        (if (ne res 0)
            ((call puts (call error_status_msg))
             (call puts call_str)
             (call print res)
             (call exit res))))

    (define ((read_module void) (buf (ptr int8)))
        ;; TODO [string]: define message string here
        (call puts (call reading_stdin_msg))

        (declare c int8)
        (while (ne (set c (call getchar))
                   ;; TODO [macro]: use 'EOF' inline
                   (call eof_char))
                (store buf c)
                (set buf (ptradd buf 1)))
        ;; TODO: set null character inline
        (store buf (call null_char))
        (ret))

    ;; [WIP]
    (define ((init_ void)
             (devCount_p (ptr int))
             (device_p (ptr int)) ; CUdevice <=> int
             (context_p (ptr (ptr int))) ; We never use the struct
             (module_p (ptr (ptr int))) ; We never use the struct
             (kernel_func_p (ptr (ptr int)))) ; We never use the struct

        ;; CUDA initialization and context creation
        ;; TODO [macro]: Wrap API calls with error-checking macros
        (call cuInit 0)
        (call cuDeviceGetCount devCount_p)
        (call cuDeviceGet device_p 0)
        (call cuCtxCreate context_p 0 (load device_p))

        ;; Load the kernel image and get a handle to the kernel function
        (declare kernel_ptx (ptr int8))
        (set kernel_ptx (alloc int8 4000))
        (call read_module kernel_ptx)
        (call cuModuleLoadData module_p kernel_ptx)
        ;; TODO [string]: define kernel name inline
        (call cuModuleGetFunction kernel_func_p (load module_p) (call kernel_name_str))
        ))

    ;; [WIP]
    (define ((run void) (kernel_func_p (ptr (ptr int))) (module_p (ptr (ptr int))) (context (ptr (ptr int))))
        ;; Allocate input and result
        (declare N int)
        (set N 1024)
        (declare a (ptr float))
        (declare b (ptr float))
        (declare res_device (ptr float))
        (declare res_host (ptr float))
        (set a (malloc (mul 4 N)))
        (set b (malloc (mul 4 N)))
        (set res_device (malloc (mul 4 N)))
        (set res_host (malloc (mul 4 N)))

        (declare i int)
        (for ((set i 0)
              (lt i N)
              (set i (add i 1)))
            (store (ptradd a i) (call rand))
            (store (ptradd b i) (call rand)))

        ;; Run the reference implementation
        (call ref_kernel a b res_host N)

        ;; Copy data to device
        (declare dev_a (ptr int))
        (declare dev_b (ptr int))
        (declare dev_res (ptr int))
        (call cuMemAlloc dev_a sizeof(float) * N));
        (call cuMemAlloc &dev_b, sizeof(float) * N));
        (call cuMemAlloc &dev_res, sizeof(float) * N));
        (call cuMemcpyHtoD dev_a, a, sizeof(float)*N));
        (call cuMemcpyHtoD dev_b, b, sizeof(float)*N));
