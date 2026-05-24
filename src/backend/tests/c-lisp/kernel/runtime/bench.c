#include <stdio.h>
#include <stdlib.h>

/**
 * @brief Calculates the number of floating point operations per second (FLOPS).
 * 
 * This function calculates the number of floating-point operations executed per second
 * by the matrix multiplication kernel using the formula `2 * m * n * k`. The parameters 
 * m, n, and k represent the leading dimensions of the input matrices and the result matrix, 
 * respectively.
 * 
 * @param[in] m Leading dimension of the first input array (number of rows).
 * @param[in] n Leading dimension of the second input array (number of columns).
 * @param[in] k Leading dimension of the resultant array.
 * @param[in] time Time taken for the matrix multiplication in seconds.
 * 
 * @return Returns the number of floating point operations per second (in GFLOPS, magnitude of 1E+9).
 *         If the time is 0, the function returns 0 instead of NaN.
 * 
 * @note If `time` is too small (e.g., close to 0), it may indicate that the matrix multiplication 
 *       was too fast or does not fit the precision level, in which case the function will return 0.
 */

double flops(int m, int n, int k, double time) {
    double num_ops = 2.0 * (double)m * (double)n * (double)k;
    double result = num_ops / (time * 1e9);
    return isnan(result) ? 0 : result;
}

