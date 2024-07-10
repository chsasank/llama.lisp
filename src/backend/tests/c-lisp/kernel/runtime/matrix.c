#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

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

bool all_close(float* A, float* B, int rows, int cols, float rtol, float atol, bool equal_NaN) {
    float RHS, LHS;
    bool* results = (bool*)malloc(rows * cols * sizeof(bool));
    bool result;

    for (int i = 0; i < cols * rows; i++) {
        LHS = fabsf(A[i] - B[i]);
        RHS = atol + rtol * fabsf(B[i]);
        
        result = LHS <= RHS;
        // result = result & compare_matrix(A, B, rows, cols);/* Increasing time taken to compute all_close */
        result = result & isfinite(B[i]);

        if(equal_NaN) 
            result |= isnan(A[i]) & isnan(B[i]);

        results[i] = result;
    }
    
    result = true;
    for(int i = 0; i < rows * cols; i++) 
        result &= results[i];

    return result;
}
