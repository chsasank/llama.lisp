#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sys/time.h>

#include "parameters.h"
#include "print.c"
#include "bench.c"

int main(int argc, char* argv[]){
    int m, n, k;

    char* routine = argv[1];
    int print_mat = atoi(argv[2]);

    if(!strcmp(routine, "once")) {
        m = M;
        n = N;
        k = K;  
    
        float* A          =    (float*)malloc(m * k * sizeof(float));
        float* B          =    (float*)malloc(k * n * sizeof(float));
        float* C_kernel   =    (float*)malloc(m * n * sizeof(float));
        float* C_ref      =    (float*)malloc(m * n * sizeof(float));

        random_matrix(A, m, k);
        random_matrix(B, k, n);

        clock_t ref_start = clock();
        kernel(A, B, C_ref, m, n, k);
        clock_t ref_end = clock();

        clock_t kernel_start = clock();
        __kernel(A, B, C_kernel, m, n, k);
        clock_t kernel_end = clock();


        double elapsed_kernel = (double)(kernel_end - kernel_start) / CLOCKS_PER_SEC;
        double elapsed_ref = (double)(ref_end - ref_start) / CLOCKS_PER_SEC;

        float kernel_flops = flops(m, n, k, elapsed_kernel);
        float ref_flops = flops(m, n, k, elapsed_ref);

        bool isequal = compare_matrix((float*)C_kernel, (float*)C_ref, m, n);
        bool allclose = all_close((float*)C_kernel, (float*)C_ref, m, n, 1e3, 1e5, true);

        printf("%d, %.3f, %.3f\n", allclose, ref_flops, kernel_flops);

        if (print_mat) {
            printf("A\n");
            print_matrix(A, m, k);
            printf("B\n");
            print_matrix(B, k, n);
            printf("Reference\n");
            print_matrix(C_ref, m, n);
            printf("Kernel\n");
            print_matrix(C_kernel, m, n);
        }

        fflush(stdout);

        free((float*)A);
        free((float*)B);
        free((float*)C_kernel);
        free((float*)C_ref);
    
    } else if (!strcmp(argv[1], "many")) {
        for(int size = START; size <= END; size += STEP) {
           
            m = n = k = size;

            for(int j = 0; j < SAMPLES; j++){
                float* A          =    (float*)malloc(m * k * sizeof(float));
                float* B          =    (float*)malloc(k * n * sizeof(float));
                float* C_kernel   =    (float*)malloc(m * n * sizeof(float));
                float* C_ref      =    (float*)malloc(m * n * sizeof(float));

                random_matrix(A, m, k);
                random_matrix(B, k, n);


                clock_t ref_start = clock();
                kernel(A, B, C_ref, m, n, k);
                clock_t ref_end = clock();

                clock_t kernel_start = clock();
                __kernel(A, B, C_kernel, m, n, k);
                clock_t kernel_end = clock();

                double elapsed_kernel = (double)(kernel_end - kernel_start) / CLOCKS_PER_SEC;
                double elapsed_ref = (double)(ref_end - ref_start) / CLOCKS_PER_SEC;

                float kernel_flops = flops(m, n, k, elapsed_kernel);
                float ref_flops = flops(m, n, k, elapsed_ref);

                bool isequal = compare_matrix((float*)C_kernel, (float*)C_ref, m, n);
                bool allclose = all_close((float*)C_kernel, (float*)C_ref, m, n, 1e3, 1e5, true);

                printf("%d, %d, %.3f, %.3f\n", size,  allclose, ref_flops, kernel_flops);
                
                fflush(stdout);

                free((float*)A);
                free((float*)B);
                free((float*)C_kernel);
                free((float*)C_ref);
            }
        }

    } else { printf("Invalid Input Argument\n"); }

    return 0;
}