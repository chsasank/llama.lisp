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

void add_dot(int k, float* a, int incx ,float* b, float* c) {
    float a_value, b_value;
    float acc = 0.;

    for(int p = 0; p < k; p++) {
        a_value = a[p * incx];
        b_value = b[p];
        acc += a_value * b_value;
    }
    c[0] = acc;
}

// int ref_MMult1(float* A, float* B, float* C, int m, int n, int k) {
//     for (int i = 0; i < m; i++) {
//         for (int j = 0; j < n; j++) {
//             float sum = 0.0;
//             for (int p = 0; p < k; p++) {
//                 sum += A[p * m + i] * B[j * k + p];
//             }
//             C[j * m + i] = sum;
//         }
//     }
//     return 1;
// }

// void add_dot1x4(int k, float* a, int lda, float* b, int ldb, float* c, int ldc) {
//     c[0 * ldc] = 0.;
//     c[1 * ldc] = 0.;
//     c[2 * ldc] = 0.;
//     c[3 * ldc] = 0.;

//     int p;

//     for(p = 0; p < k; p++)
//         c[0 * ldc] += a[p * lda] * b[p + ldb * 0];

//     for(p = 0; p < k; p++)
//         c[1 * ldc] += a[p * lda] * b[p + ldb * 1];

//     for(p = 0; p < k; p++)
//         c[2 * ldc] += a[p * lda] * b[p + ldb * 2];

//     for(p = 0; p < k; p++)
//         c[3 * ldc] += a[p * lda] * b[p + ldb * 3];

// }       

// void ref_MMult_1x4(int m, int n, int k, int lda, int ldb, int ldc, float* a, float* b, float* c) {
//     for(int j = 0; j < n; j += 4) 
//         for(int i = 0; i < m; i++) 
//             add_dot1x4(k, &a[i], lda, &b[j * ldb], ldb, &c[j * ldc + i], ldc);
// }

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
