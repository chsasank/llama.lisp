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
    float time, elapsed;

    m = 100;
    n = 100;
    k = 100;

    float* A = (float*)malloc(m * k * sizeof(float));
    float* B = (float*)malloc(k * n * sizeof(float));
    float* C_kernel = (float*)malloc(m * n * sizeof(float));
    float* C_ref = (float*)malloc(m * n * sizeof(float));

    random_matrix(A, m, k);
    random_matrix(B, k, n);

    time = -timer();
    // __MMult1(A, B, C_kernel, m, n, k);
    time += timer();

    elapsed = ((double)time / CLOCKS_PER_SEC);

    ref_mult(A, B, C_ref, m, n, k);
    float kernel_flops = flops(m, n, k, elapsed);

    // print_matrix(A, m, k);
    // print_matrix(B, k, n);
    // print_matrix(C_kernel, m, n);
    // print_matrix(C_ref, m, n);

    printf("%d\n", compare_matrix(C_kernel, C_ref, m, n));
    printf("%.4fms %.4f Gflops\n", elapsed * 1e3, kernel_flops);

    return 0;
}