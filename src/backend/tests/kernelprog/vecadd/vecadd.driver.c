#include <stdlib.h>
#include <stdio.h>
#include <cuda.h>
#include <math.h>

#define CLISP_TRANS

/* Host-side code to launch the kernel function and
   validate the results against those of a reference function */

#ifdef CLISP_TRANS
char * ReadingStdinMsg = "Reading kernel from standard input...";
char * KernelNameStr = "kernel";
char * ErrorStatusMsg = "Non-zero return status";
char * reading_stdin_msg () {
    return ReadingStdinMsg;
}
char * kernel_name_str () {
    return KernelNameStr;
}
char * error_status_msg () {
    return ErrorStatusMsg;
}
char eof_char () {
    return EOF;
}
char null_char () {
    return '\0';
}
void print (int n) {
    printf("%d\n", n);
}
#endif

#ifndef CLISP_TRANS
// Reference implementation, defined per kernel
void ref_kernel(float * a, float * b, float * res, int N) {
    for (int i = 0; i < N; i++)
        res[i] = a[i] + b[i];
}
#else
void ref_kernel(float * a, float * b, float * res, int N);
#endif

// CUDA error checking
#define ERR_CHECK(X) \
    error_check(X, #X)
#ifndef CLISP_TRANS
void error_check(int res, char * call_str) {
    if (res) {
        printf("%s returned non-zero status %d\n", call_str, res);
        exit(1);
    }
}

#else
void error_check(int res, char * call_str);
#endif

// Read a PTX image containing a kernel from stdin
#ifndef CLISP_TRANS
void read_module(char * buf) {
    printf("Reading kernel from standard input...\n");

    char c;
    while ((c = getchar()) != EOF) {
        *(buf++) = c;
    }
    *buf = '\0';
}
#else
void read_module(char * buf);
#endif

#ifndef _CLISP_TRANS // [WIP]
void init (int * devCount_p, CUdevice * device_p,
           CUcontext * context_p, CUmodule * module_p,
           CUfunction * kernel_func_p) {

    // CUDA initialization and context creation
    ERR_CHECK(cuInit(0));
    ERR_CHECK(cuDeviceGetCount(devCount_p));
    ERR_CHECK(cuDeviceGet(device_p, 0));
    ERR_CHECK(cuCtxCreate(context_p, 0, *device_p));

    // Load the kernel image and get a handle to the kernel function
    char kernel_ptx[4000];
    read_module(kernel_ptx);
    ERR_CHECK(cuModuleLoadData(module_p, kernel_ptx));
    ERR_CHECK(cuModuleGetFunction(kernel_func_p, *module_p, "kernel"));
}
#else
void init (int * devCount_p, CUdevice * device_p,
           CUcontext * context_p, CUmodule * module_p,
           CUfunction * kernel_func_p);
#endif

void run (CUfunction * kernel_func_p, CUmodule * module_p, CUcontext * context_p) {
    // Allocate input and result
    int N = 1024;
    float
        *a = malloc(sizeof(float) * N),
        *b = malloc(sizeof(float) * N),
        *res_device = malloc(sizeof(float) * N),
        *res_host = malloc(sizeof(float) * N);

    for (int i = 0; i < N; i++) {
        a[i] = rand();
        b[i] = rand();
    }
    
    // Run the reference implementation
    ref_kernel(a, b, res_host, N);

    // Copy data to the device
    CUdeviceptr dev_a, dev_b, dev_res;
    ERR_CHECK(cuMemAlloc(&dev_a, sizeof(float) * N));
    ERR_CHECK(cuMemAlloc(&dev_b, sizeof(float) * N));
    ERR_CHECK(cuMemAlloc(&dev_res, sizeof(float) * N));
    ERR_CHECK(cuMemcpyHtoD(dev_a, a, sizeof(float)*N));
    ERR_CHECK(cuMemcpyHtoD(dev_b, b, sizeof(float)*N));

    // Launch the kernel and wait
    void * KernelParams [] = { &dev_a, &dev_b, &dev_res };
    int BlockSize = 32;
    int GridSize = (N + BlockSize - 1) / BlockSize;
    ERR_CHECK(cuLaunchKernel(*kernel_func_p,
                             // Grid sizes X, Y, Z
                             GridSize, 1, 1,
                             // Block sizes X, Y, Z
                             BlockSize, 1, 1,
                             // shared mem size, stream id, kernel params, extra options
                             0, NULL, KernelParams, NULL));
    ERR_CHECK(cuCtxSynchronize());

    // Retrieve and verify results
    ERR_CHECK(cuMemcpyDtoH(res_device, dev_res, sizeof(float) * N));
    float max_err = 0.0;
    for (int i = 0; i < N; i++)
      max_err = fmax(max_err,
                     fabs(res_host[i] - res_device[i]));
    printf("Max error: %f\n", max_err);

    // Cleanup
    free(a);
    free(b);
    free(res_device);
    free(res_host);
    ERR_CHECK(cuMemFree(dev_a));
    ERR_CHECK(cuMemFree(dev_b));
    ERR_CHECK(cuMemFree(dev_res));
    ERR_CHECK(cuModuleUnload(*module_p));
    ERR_CHECK(cuCtxDestroy(*context_p));
}

int main () {
    int devCount;
    CUdevice device;
    CUcontext context;
    CUmodule module;
    CUfunction kernel_func;

    init(&devCount, &device, &context, &module, &kernel_func);
    run(&kernel_func, &module, &context);
}
