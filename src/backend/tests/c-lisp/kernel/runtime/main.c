#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sys/time.h>

#include "print.c"
// #include "matrix.c"
#include "bench.c"

int main(){
    int m, n, k;

    // m = 2000;
    // n = 2000;
    // k = 2000;
    
    printf("size, is_equal, kernel_time, kernel_flops, ref_time, ref_flops\n");
    int size = 0;
    for (size = 0; size <= 2000; size += 200){
        m = size;
        n = size;
        k = size;

        float* A = (float*)malloc(m * k * sizeof(float));
        float* B = (float*)malloc(k * n * sizeof(float));
        float* C_kernel = (float*)malloc(m * n * sizeof(float));
        float* C_ref = (float*)malloc(m * n * sizeof(float));

        random_matrix(A, m, k);
        random_matrix(B, k, n);


        clock_t ref_start = clock();
        ref_mult(A, B, C_ref, m, n, k);
        // ref_mult(A, B, C_ref, m, n, k);
        // ref_mult(A, B, C_ref, m, n, k);
        clock_t ref_end = clock();

        clock_t kernel_start = clock();
        // __MMult1(A, B, C_kernel, m, n, k);
        // __MMult1(A, B, C_kernel, m, n, k);
        __MMult1(A, B, C_kernel, m, n, k);
        clock_t kernel_end = clock();


        bool isequal = compare_matrix(C_kernel, C_ref, m, n);

        double elapsed_kernel = (double) (kernel_end - kernel_start) / CLOCKS_PER_SEC;
        double elapsed_ref = (double) (ref_end - ref_start) / CLOCKS_PER_SEC;

        float kernel_flops = flops(size, size, size, elapsed_kernel);
        float ref_flops = flops(size, size, size, elapsed_ref);

        printf("%d, %d, %.3f, %.3f, %.3f, %.3f\n",
                size, isequal,
                elapsed_kernel,
                kernel_flops,
                elapsed_ref,
                ref_flops);
    }

    // volatile float* A          =    (float*)malloc(m * k * sizeof(float));
    // volatile float* B          =    (float*)malloc(k * n * sizeof(float));
    // volatile float* C_kernel   =    (float*)malloc(m * n * sizeof(float));
    // volatile float* C_ref      =    (float*)malloc(m * n * sizeof(float));

    // clock_t ref_start = clock();
    // ref_mult((float*)A, (float*)B, (float*)C_ref, m, n, k);
    // clock_t ref_end = clock();

    // clock_t kernel_start = clock();
    // __MMult1((float*)A, (float*)B, (float*)C_kernel, m, n, k);
    // clock_t kernel_end = clock();

    // double elapsed_kernel = (double)(kernel_end - kernel_start) / CLOCKS_PER_SEC;
    // double elapsed_ref = (double)(ref_end - ref_start) / CLOCKS_PER_SEC;

    // float kernel_flops = flops(m, n, k, elapsed_kernel);
    // float ref_flops = flops(m, n, k, elapsed_ref);

    // bool isequal = compare_matrix((float*)C_kernel, (float*)C_ref, m, n);

    // printf("kernel flops : %.3f\n", kernel_flops);
    // printf("ref flops    : %.3f\n", ref_flops);

    // free((float*)A);
    // free((float*)B);
    // free((float*)C_kernel);
    // free((float*)C_ref);

    return 0;
}