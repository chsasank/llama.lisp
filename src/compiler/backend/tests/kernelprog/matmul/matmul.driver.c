#include <stdlib.h>
#include <stdio.h>
#include <cuda.h>
#include <math.h>

/* Host-side code to launch the kernel function and
   validate the results against those of a reference function */

// Reference implementation, defined per kernel
void ref_kernel(float * a, float * b, float * res, int N) {
  for (int i = 0; i < N; i++)
    for (int j = 0; j < N; j++) {
      res[i * N + j] = 0;
      for (int k = 0; k < N; k++)
        res[i * N + j] += a[i * N + k] * b[k * N + j];
    }
}

// CUDA error checking
#define ERR_CHECK(X) \
    error_check(X, #X)
void error_check(int res, char * call_str) {
    if (res) {
        printf("%s returned non-zero status %d\n", call_str, res);
        exit(1);
    }
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
    char kernel_ptx[5000];
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
    int N = 256;
    int len = N * N;
    float
        *a = malloc(sizeof(float) * len),
        *b = malloc(sizeof(float) * len),
        *res_device = malloc(sizeof(float) * len),
        *res_host = malloc(sizeof(float) * len);

    for (int i = 0; i < len; i++) {
        a[i] = rand();
        b[i] = rand();
    }
    
    // Run the reference implementation
    ref_kernel(a, b, res_host, N);

    // Copy data to the device
    CUdeviceptr dev_a, dev_b, dev_res;
    ERR_CHECK(cuMemAlloc(&dev_a, sizeof(float) * len));
    ERR_CHECK(cuMemAlloc(&dev_b, sizeof(float) * len));
    ERR_CHECK(cuMemAlloc(&dev_res, sizeof(float) * len));
    ERR_CHECK(cuMemcpyHtoD(dev_a, a, sizeof(float) * len));
    ERR_CHECK(cuMemcpyHtoD(dev_b, b, sizeof(float) * len));

    // Launch the kernel and wait
    void * KernelParams [] = { &dev_a, &dev_b, &dev_res, &N };
    int BlockSize = 32;
    int GridSize = (N + BlockSize - 1) / BlockSize;
    ERR_CHECK(cuLaunchKernel(kernel_func,
                             // Grid sizes X, Y, Z
                             GridSize, GridSize, 1,
                             // Block sizes X, Y, Z
                             BlockSize, BlockSize, 1,
                             // shared mem size, stream id, kernel params, extra options
                             0, NULL, KernelParams, NULL));
    ERR_CHECK(cuCtxSynchronize());

    // Retrieve and verify results
    ERR_CHECK(cuMemcpyDtoH(res_device, dev_res, sizeof(float) * len));
    float max_err = 0.0;
    for (int i = 0; i < len; i++)
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
