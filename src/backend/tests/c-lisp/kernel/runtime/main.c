#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sys/time.h>

#include "print.c"
#include "matrix.c"
#include "bench.c"

int main(){
    int m, n, k;

    
    printf("size,is_equal,kernel_time,ref_time\n");
    int size = 0;
    for (size = 0; size < 8000; size += 200){
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
        ref_mult(A, B, C_ref, m, n, k);
        ref_mult(A, B, C_ref, m, n, k);

        clock_t ref_end = clock();

        clock_t kernel_start = clock();
        __MMult1(A, B, C_kernel, m, n, k);
        __MMult1(A, B, C_kernel, m, n, k);
        __MMult1(A, B, C_kernel, m, n, k);
        clock_t kernel_end = clock();


        bool isequal = compare_matrix(C_kernel, C_ref, m, n);

        double elapsed_kernel = (double) (kernel_end - kernel_start) / CLOCKS_PER_SEC;
        double elapsed_ref = (double) (ref_end - ref_start) / CLOCKS_PER_SEC;

        printf("%d, %d, %.3f, %.3f\n",
                size, isequal,
                elapsed_kernel / 3.,
                elapsed_ref / 3.);
    }

    return 0;
}