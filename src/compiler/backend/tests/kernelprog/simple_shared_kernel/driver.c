#include <cuda.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define CU_CHECK(err) do { if ((err) != CUDA_SUCCESS) { \
    const char *msg; cuGetErrorString(err, &msg); \
    fprintf(stderr, "CUDA ERROR: %s\n", msg); exit(1); } \
} while (0)


int main(int argc, char **argv)
{
    if (argc != 2) {
        fprintf(stderr, "Usage: %s kernel.ptx\n", argv[0]);
        return 1;
    }

    const char *ptx_file = argv[1];

    CUdevice device;
    CUcontext context;
    CUmodule module;
    CUfunction kernel;

    CU_CHECK(cuInit(0));
    CU_CHECK(cuDeviceGet(&device, 0));
    CU_CHECK(cuCtxCreate(&context, 0, device));

    FILE *f = fopen(ptx_file, "rb");
    if (!f) {
        fprintf(stderr, "Failed to open %s: %s\n",
                ptx_file, strerror(errno));
        return 1;
    }

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    rewind(f);

    char *ptx = (char *)malloc(size + 1);
    fread(ptx, 1, size, f);
    fclose(f);
    ptx[size] = '\0';

    CU_CHECK(cuModuleLoadData(&module, ptx));
    free(ptx);

    CU_CHECK(cuModuleGetFunction(&kernel, module, "kernel"));

    const int N = 64;
    int h[N];

    for (int i = 0; i < N; i++)
        h[i] = i;

    CUdeviceptr d;
    CU_CHECK(cuMemAlloc(&d, sizeof(int) * N));
    CU_CHECK(cuMemcpyHtoD(d, h, sizeof(int) * N));

    void *args[] = {
        &d,
        (void *)&N
    };

    CU_CHECK(cuLaunchKernel(
        kernel,
        1, 1, 1,        // grid
        64, 1, 1,       // block (64 threads)
        0,              // dynamic shared memory (0, static only)
        0,              // default stream
        args,
        0
    ));

    CU_CHECK(cuCtxSynchronize());
     
    CU_CHECK(cuMemcpyDtoH(h, d, sizeof(int) * N));

    int correct = 1;
    for (int i = 0; i < N; i++) {
        if (h[i] != (N - 1 - i)) {
            correct = 0;
            break;
        }
    }

    if (correct) {
        printf("kernel PASSED\n");
    } else {
        printf("kernel FAILED\n");
        for (int i = 0; i < N; i++)
            printf("%d ", h[i]);
        printf("\n");
    }

    CU_CHECK(cuMemFree(d));
    CU_CHECK(cuModuleUnload(module));
    CU_CHECK(cuCtxDestroy(context));

    return 0;
}
