#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

/**
 * @brief Generates a random matrix
 * 
 * This function generates a matrix with random floating point entries
 * 
 * @param[out] matrix Address to the location of the matrix to be initialized
 * @param[in] rows The number of rows in the matrix
 * @param[in] cols The number of columns in the matrix
 * 
 * @note The function assumes the matrix is in column-major format
 * 
 * @return Returns 1 so that the compiler does not discard the function call during optimization
 */

int random_matrix(float* matrix, int rows, int cols) {
    srand((unsigned int)time(NULL));
    for (int j = 0; j < cols; j++) {
        for (int i = 0; i < rows; i++) {
            matrix[j * rows + i] = 3.0 * ((float)rand() / RAND_MAX) - 1.0;
        }
    }

    return 1;
}

/**
 * @brief Compares the elements of the two input matrixes to check if the are exactly equal
 * 
 * The function iterates through each element in the matrix individually checking of the elements 
 * are exactly equal. returns a boolean value 
 * 
 * @param[in] res The resultant matrix of matrix multiplication
 * @param[in] ref The matrix which is already determined to be the correct resultant matrix 
 *                when the two matrices are multiplied
 * @param[in] rows Number of rows in the matrices
 * @param[in] cols Number of columns in the matrices
 * 
 * @note No longer used as a better function exists
 * 
 * @returns Boolean value. True if the matrices are equal elementwise. False otherwise
 */

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

/**
 * @brief Checks if two matrices are element-wise close within specified tolerances.
 *
 * This function compares two matrices `A` and `B` to determine if they are element-wise 
 * close, given relative and absolute tolerances (`rtol` and `atol`). Additionally, it can
 * handle `NaN` values depending on the `equal_NaN` flag.
 * 
 * The comparison is based on the formula:
 * | A[i] - B[i] | = atol + rtol * B[i]
 * where `atol` is the absolute tolerance and `rtol` is the relative tolerance.
 * 
 * @param[in] A         First matrix (flattened array).
 * @param[in] B         Second matrix (flattened array).
 * @param[in] rows      Number of rows in the matrices.
 * @param[in] cols      Number of columns in the matrices.
 * @param[in] rtol      Relative tolerance.
 * @param[in] atol      Absolute tolerance.
 * @param[in] equal_NaN If true, considers `NaN` values in `A` and `B` as equal.
 * 
 * @return Returns `true` if all corresponding elements in the matrices are close within
 *         the specified tolerances. Otherwise, returns `false`.
 *
 * @note The function allocates memory for a temporary results array, which is freed at the end.
 */
bool all_close(float* A, float* B, int rows, int cols, float rtol, float atol, bool equal_NaN) {
    float RHS, LHS;
    bool* results = (bool*)malloc(rows * cols * sizeof(bool));  // Array to store comparison results.
    bool final_result;

    // Compare each element in matrices A and B.
    for (int i = 0; i < cols * rows; i++) {
        LHS = fabsf(A[i] - B[i]);                 // Left-hand side: |A[i] - B[i]|
        RHS = atol + rtol * fabsf(B[i]);           // Right-hand side: atol + rtol * |B[i]|
        
        // Element-wise comparison.
        final_result = (LHS <= RHS);
        final_result = final_result && isfinite(B[i]);  // Ensure B[i] is finite.

        // Handle NaN comparison if equal_NaN is set.
        if (equal_NaN) {
            final_result = final_result || (isnan(A[i]) && isnan(B[i]));
        }

        results[i] = final_result;  // Store the result for each comparison.
    }
    
    // Check if all elements satisfy the condition.
    final_result = true;
    for (int i = 0; i < rows * cols; i++) {
        final_result = final_result && results[i];
    }

    free(results);  // Free allocated memory.
    return final_result;
}
