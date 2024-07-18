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
        
        result = (LHS <= RHS);
        result = result && isfinite(B[i]);

        if (equal_NaN) {
            result = result || (isnan(A[i]) && isnan(B[i]));
        }

        results[i] = result;
    }
    
    result = true;
    for(int i = 0; i < rows * cols; i++) 
        result = result && results[i];

    free(results);
    return result;
}
