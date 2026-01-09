#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

/**
 * @brief Prints an integer value to the standard output.
 * 
 * This function takes an integer `x`, prints it, and returns a large constant value 
 * to prevent compiler optimization.
 * 
 * @param[in] x The integer value to print.
 * @return A large constant integer value (0x7ffffff) to prevent optimization.
 */
int print(int x){
    printf("%d\n", x);
    return 0x7ffffff;
}

/**
 * @brief Prints a floating-point value to the standard output.
 * 
 * This function takes a float `x`, prints it formatted to two decimal places, and 
 * returns a large constant value to prevent compiler optimization.
 * 
 * @param[in] x The floating-point value to print.
 * @return A large constant integer value (0x7fffffff) to prevent optimization.
 */
int fprint(float x) {
    printf("%.2f\n", x);
    return 0x7fffffff;
}

/**
 * @brief Prints a character to the standard output.
 * 
 * This function takes an integer representing an ASCII character, prints it, and 
 * returns a large constant value to prevent compiler optimization.
 * 
 * @param[in] character The integer value of the character to print.
 * @return A large constant integer value (0x7fffffff) to prevent optimization.
 */
int print_char(int character) {
    printf("%c", character);
    return 0x7fffffff; 
}

/**
 * @brief Prints a matrix (2D array) to the standard output.
 * 
 * This function takes a pointer to a matrix (1D array) `matrix`, with its dimensions 
 * specified by `rows` and `cols`, and prints it in a readable format. The matrix is 
 * stored in column-major order.
 * 
 * @param[in] matrix Pointer to the matrix (1D array) to print.
 * @param[in] rows Number of rows in the matrix.
 * @param[in] cols Number of columns in the matrix.
 * @return A large constant integer value (0x7fffffff) to prevent optimization.
 */
int print_matrix(float* matrix, int rows, int cols) {
    for (int j = 0; j < cols; j++) {
        for (int i = 0; i < rows; i++) {
            printf("%.5f ", matrix[j * rows + i]);
        }
        printf("\n");
    }
    printf("\n");
    return 0x7fffffff;
}
