#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <stdint.h>

#define A(i,j) a[ (j)*lda + (i) ]
#define B(i,j) b[ (j)*ldb + (i) ]
#define C(i,j) c[ (j)*ldc + (i) ]

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

int print_matrix(float* matrix, int rows, int cols) {
    for (int j=0; j<cols; j++ ){
        for (int i=0; i<rows; i++ ){
            printf("%.2f ", matrix[j * rows + i]);
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

int compare_matrix(float* res, float* ref, int rows, int cols) {
    for (int j = 0; j < cols; j++) {
        for (int i = 0; i < rows; i++) {
            if (res[j * rows + i] != ref[j * rows + i]) {
                return 0;
            }
        }
    }
    return 1;
}




