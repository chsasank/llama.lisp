/**
 * @file parameters.h
 * @brief Defines constants used for matrix size and benchmarking configurations.
 *
 * This header file contains constants used throughout the matrix multiplication
 * benchmarking process, including matrix dimensions and benchmark settings such
 * as sample size and step increments.
 */

#ifndef PARAMETERS_H
#define PARAMETERS_H

/**
 * @brief Starting matrix size for benchmarking in the "many" mode.
 *
 * This constant defines the smallest matrix size used when benchmarking
 * different matrix multiplication operations. It is the starting value in the loop
 * that increments the matrix size for each test.
 */
#define START 400

/**
 * @brief Maximum matrix size for benchmarking in the "many" mode.
 *
 * This constant defines the largest matrix size used when benchmarking matrix
 * multiplication operations. The tests will stop when this size is reached.
 */
#define END 2000

/**
 * @brief Step size for increasing the matrix size during benchmarking in the "many" mode.
 *
 * This constant specifies the amount by which the matrix size is increased
 * after each test during the benchmarking process.
 */
#define STEP 400

/**
 * @brief Number of samples per matrix size during benchmarking.
 *
 * This constant defines how many times the matrix multiplication will be
 * repeated for each matrix size in the benchmarking process.
 */
#define SAMPLES 10

/**
 * @brief Matrix dimensions for the "once" mode.
 *
 * These constants define the size of the matrices used for a single run
 * of matrix multiplication (when the program is run in the "once" mode).
 * - M: Number of rows in the first matrix (A) and the result matrix (C).
 * - N: Number of columns in the second matrix (B) and the result matrix (C).
 * - K: Number of columns in the first matrix (A) and rows in the second matrix (B).
 */
#define M 8    /**< Number of rows in matrix A and C. */
#define N 8    /**< Number of columns in matrix B and C. */
#define K 8    /**< Number of columns in matrix A and rows in matrix B. */

#endif // PARAMETERS_H
