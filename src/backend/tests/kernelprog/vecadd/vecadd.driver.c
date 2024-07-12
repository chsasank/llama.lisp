#include <stdlib.h>
#include <stdio.h>
#include <cuda.h>
#include <math.h>

/* Host-side code to launch an arbitrary kernel function and
   validate the results against those of a reference function */

// Reference implementation, defined per kernel
void ref_kernel(float * a, float * b, float * res, int N) {
    for (int i = 0; i < N; i++)
        res[i] = a[i] + b[i];
}

// CUDA error checking
#define ERR_CHECK(X) \
    error_check(X, #X)
void error_check(int res, char * call_str) {
    if (res)
        printf("%s returned non-zero status %d", call_str, res);
}

// Read a PTX image containing a kernel from stdin
void read_module(char * buf) {
    printf("Reading kernel from standard input...\n");

    char c;
    while ((c = getchar()) != EOF) {
        *(buf++) = c;
    }
    *buf = '\0';
}

int main (int argc, char ** argv) {
    int devCount;
    CUdevice device;
    CUcontext context;
    CUmodule module;
    char kernel_ptx[4000];
    CUfunction kernel_func;
      
    // CUDA initialization and context creation
    ERR_CHECK(cuInit(0));
    ERR_CHECK(cuDeviceGetCount(&devCount));
    ERR_CHECK(cuDeviceGet(&device, 0));
    ERR_CHECK(cuCtxCreate(&context, 0, device));

    // Load the kernel image and get a handle to the kernel function
    read_module(kernel_ptx);
    ERR_CHECK(cuModuleLoadData(&module, kernel_ptx));
    ERR_CHECK(cuModuleGetFunction(&kernel_func, module, "kernel"));

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
    ERR_CHECK(cuLaunchKernel(kernel_func,
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
    ERR_CHECK(cuModuleUnload(module));
    ERR_CHECK(cuCtxDestroy(context));
}
