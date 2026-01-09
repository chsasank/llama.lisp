#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sys/time.h>

#include "parameters.h"
#include "print.c"
#include "bench.c"

// Function to print usage instructions
void print_usage() {
    printf("Usage: ./program_name <routine> <print_mat>\n");
    printf("routine: 'once' for a single run, 'many' for benchmarking with multiple sizes.\n");
    printf("print_mat: 0 to not print matrices, 1 to print matrices.\n");
}

/**
 * @brief Main function to benchmark matrix multiplication kernels.
 * 
 * The program supports two modes of operation:
 * - "once": Runs matrix multiplication for fixed dimensions and compares a reference kernel with a custom kernel.
 * - "many": Runs matrix multiplication for different matrix sizes in a specified range and performs comparisons across multiple samples.
 * 
 * The program computes the FLOPS (floating-point operations per second) for both the reference and custom kernels, 
 * compares the output matrices, and prints the results.
 * 
 * Command-line arguments:
 * @param[in] argc Number of arguments passed to the program.
 * @param[in] argv[] Array of argument strings.
 * 
 * @param[in] argv[1] "once" or "many" to specify the mode of operation.
 * @param[in] argv[2] If non-zero, prints matrices (only applicable in "once" mode).
 * 
 * The program uses matrices of floats and performs the following operations:
 * - Generates random matrices `A` and `B`.
 * - Executes both a reference matrix multiplication kernel (`kernel`) and a custom kernel (`__kernel`).
 * - Measures the time taken by both kernels.
 * - Calculates and prints the FLOPS for both kernels.
 * - Compares the outputs from both kernels using `compare_matrix` and `all_close`.
 * - Optionally prints the matrices if the `print_mat` flag is set.
 * 
 * In "many" mode, the program iterates over matrix sizes from `START` to `END`, incrementing by `STEP`, and repeats each experiment `SAMPLES` times.
 * 
 * @return 0 on success, or an error code on failure.
 */

int main(int argc, char* argv[]) {
    // Check if the number of arguments is correct
    if (argc != 3) {
        printf("Error: Invalid number of arguments.\n");
        print_usage();
        return 1;
    }

    int m, n, k;
    char* routine = argv[1];
    int print_mat = atoi(argv[2]);

    // Check if routine is valid
    if (strcmp(routine, "once") != 0 && strcmp(routine, "many") != 0) {
        printf("Error: Invalid routine argument.\n");
        print_usage();
        return 1;
    }

    // Check if print_mat is valid (either 0 or 1)
    if (print_mat != 0 && print_mat != 1) {
        printf("Error: Invalid print_mat argument. Must be 0 or 1.\n");
        print_usage();
        return 1;
    }

    // Routine "once" mode
    if (!strcmp(routine, "once")) {
        m = M;
        n = N;
        k = K;  

        float* A = (float*)malloc(m * k * sizeof(float));
        float* B = (float*)malloc(k * n * sizeof(float));
        float* C_kernel = (float*)malloc(m * n * sizeof(float));
        float* C_ref = (float*)malloc(m * n * sizeof(float));

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

        bool allclose = all_close(C_kernel, C_ref, m, n, 1e-3, 1e-5, true);

        printf("All close: %d, Reference FLOPS: %.3f, Kernel FLOPS: %.3f\n", allclose, ref_flops, kernel_flops);

        if (print_mat) {
            printf("Matrix A:\n");
            print_matrix(A, m, k);
            printf("Matrix B:\n");
            print_matrix(B, k, n);
            printf("Reference Matrix:\n");
            print_matrix(C_ref, m, n);
            printf("Kernel Matrix:\n");
            print_matrix(C_kernel, m, n);
        }

        free(A);
        free(B);
        free(C_kernel);
        free(C_ref);
    }

    // Routine "many" mode
    else if (!strcmp(routine, "many")) {
        for (int size = START; size <= END; size += STEP) {
            m = n = k = size;

            for (int j = 0; j < SAMPLES; j++) {
                float* A = (float*)malloc(m * k * sizeof(float));
                float* B = (float*)malloc(k * n * sizeof(float));
                float* C_kernel = (float*)malloc(m * n * sizeof(float));
                float* C_ref = (float*)malloc(m * n * sizeof(float));

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

                bool allclose = all_close(C_kernel, C_ref, m, n, 1e-3, 1e-5, true);

                printf("Size: %d, All close: %d, Reference FLOPS: %.3f, Kernel FLOPS: %.3f\n", size, allclose, ref_flops, kernel_flops);

                free(A);
                free(B);
                free(C_kernel);
                free(C_ref);
            }
        }
    }

    return 0;
}
