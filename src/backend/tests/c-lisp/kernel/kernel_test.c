#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <sys/time.h>

int print(int x){
    printf("%d\n", x);
    return x;
}

float fprint(float x) {
    printf("%.2f\n", x);
    return x;
}

uint8_t print_char(int character) {
    printf("%c", character);
    return 0xff; 
}

float flops(int m, int n, int k, float time) {
    return 2 * (m * n * k) / time / 1e9;
}

clock_t timer() {
    return clock(); 
}

int print_matrix(float* matrix, int rows, int cols) {
    for (int j=0; j<cols; j++ ){
        for (int i=0; i<rows; i++ ){
            printf("%.5f ", matrix[j * rows + i]);
        }
        printf("\n");
    }
    printf("\n");
    return 1;
}

int random_matrix(float* matrix, int rows, int cols) {
    srand((unsigned int)time(NULL));
    for (int j = 0; j < cols; j++) {
        for (int i = 0; i < rows; i++) {
            matrix[j * rows + i] = 3.0 * ((float)rand() / RAND_MAX) - 1.0;
        }
    }

    return 1;
}

int ref_mult(float* A, float* B, float* C, int m, int n, int k) {
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            float sum = 0.0;
            for (int p = 0; p < k; p++) {
                sum += A[p * m + i] * B[j * k + p];
            }
            C[j * m + i] = sum;
        }
    }
    return 1;
}

bool compare_matrix(float* res, float* ref, int rows, int cols) {
    for (int j = 0; j < cols; j++) {
        for (int i = 0; i < rows; i++) {
            if (res[j * rows + i] != ref[j * rows + i]) {
                return false;
            }
        }
    }
    return true;
}

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
    __MMult1(A, B, C_kernel, m, n, k);
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