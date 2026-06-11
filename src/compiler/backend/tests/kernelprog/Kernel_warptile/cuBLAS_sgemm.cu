//CMD: nvcc cu_BLAS_sgemm.cu -o sgemm_exec -lcublas -lcuda

#include <cstdio>
#include <cublas_v2.h>
#include <cuda_runtime.h>
#include <algorithm>
#include <cstdlib>
#include <cuda.h>


/*
 * A stand-alone script to invoke & benchmark standard cuBLAS SGEMM performance
 */

int main(int argc, char *argv[]) {
    int m = 4096;
    int k = 4096;
    int n = 4096;
    int print = 1;
    //   cudaError_t cudaStat;  // cudaMalloc status
    cublasStatus_t stat;   // cuBLAS functions status
    cublasHandle_t handle; // cuBLAS context

    int i, j;

    float *a, *b, *c;

    // malloc for a,b,c...
    a = (float *)malloc(m * k * sizeof(float));
    b = (float *)malloc(k * n * sizeof(float));
    c = (float *)malloc(m * n * sizeof(float));

    int ind = 11;
    for (j = 0; j < m * k; j++) {
        a[j] = (float)ind++;
    }

    ind = 11;
    for (j = 0; j < k * n; j++) {
        b[j] = (float)ind++;
    }

    ind = 11;
    for (j = 0; j < m * n; j++) {
        c[j] = (float)ind++;
    }

    // DEVICE
    float *d_a, *d_b, *d_c;

    // cudaMalloc for d_a, d_b, d_c...
    cudaMalloc((void **)&d_a, m * k * sizeof(float));
    cudaMalloc((void **)&d_b, k * n * sizeof(float));
    cudaMalloc((void **)&d_c, m * n * sizeof(float));

    stat = cublasCreate(&handle); // initialize CUBLAS context

    cudaMemcpy(d_a, a, m * k * sizeof(float), cudaMemcpyHostToDevice);
    cudaMemcpy(d_b, b, k * n * sizeof(float), cudaMemcpyHostToDevice);
    cudaMemcpy(d_c, c, m * n * sizeof(float), cudaMemcpyHostToDevice);

    float alpha = 1.0f;
    float beta = 0.5f;

    CUevent start, stop;
    cuEventCreate(&start, CU_EVENT_DEFAULT);
    cuEventCreate(&stop, CU_EVENT_DEFAULT);

    cuEventRecord(start, 0);

    stat = cublasSgemm(handle, CUBLAS_OP_N, CUBLAS_OP_N, n, m, k, &alpha, d_b, n,
                     d_a, k, &beta, d_c, n);
 
    if (stat != CUBLAS_STATUS_SUCCESS) {
        fprintf(stderr, "cuBLAS SGEMM failed! Status = %d\n", stat);
        exit(EXIT_FAILURE);}
    
    printf("SGEMM completed successfully\n");

    cuEventRecord(stop, 0);

    cuCtxSynchronize();
    float milliseconds = 0;
    cuEventElapsedTime(&milliseconds, start, stop);

    double seconds = milliseconds / 1000.0;
    double flops = 2.0 * (double)m * (double)n * (double)k;
    double tflops = (flops / seconds) / 1e12;

    printf("cuBLAS Time: %.3f ms\n", milliseconds);
    printf("Performance: %.4f TFLOPS\n", tflops);

    cuEventDestroy(start);
    cuEventDestroy(stop);
    
    cudaMemcpy(c, d_c, m * n * sizeof(float), cudaMemcpyDeviceToHost);

    //if you want to verify the matmul result uncomment these lines:
//   if (print == 1) {
//     printf("\nC after SGEMM = \n");
//     for (i = 0; i < m; i++) {
//       for (j = 0; j < n; j++) {
//         printf("%4.1f ", c[i * n + j]);
//       }
//       printf("\n");
//     }
//   }
 
    cudaFree(d_a);
    cudaFree(d_b);
    cudaFree(d_c);
    cublasDestroy(handle);  
    free(a);
    free(b);
    free(c);

    return EXIT_SUCCESS;
}