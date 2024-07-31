#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

int print(int x){
    printf("%d\n", x);
    return 0x7ffffff;
}

int fprint(float x) {
    printf("%.2f\n", x);
    return 0x7fffffff;
}

int print_char(int character) {
    printf("%c", character);
    return 0x7fffffff; 
}

int print_matrix(float* matrix, int rows, int cols) {
    for (int j=0; j<cols; j++ ){
        for (int i=0; i<rows; i++ ){
            printf("%.5f ", matrix[j * rows + i]);
        }
        printf("\n");
    }
    printf("\n");
    return 0x7fffffff;
}