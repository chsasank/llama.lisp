#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
int print(int x){
    printf("%d\n", x);
    return x;
}

float fprint(float x) {
    printf("%f\n", x);
    return x;
}


double dprint(double x){
    printf("%lf\n",x);
    return x;
}
//create a print for int 8 and int 16 and int 32.
// int8_t int8_print(int8_t x){
//     printf("%" PRIi8, x);
//     printf("\n");
//     return x;
// }
// int16_t int16_print(int16_t x){
//     printf("%hd",x);
//     return x;
// }
// int int32_print(int x){
//     printf("%d",x);
//     return x;

// }

// float int_to_float(int x){
//     float y = x;
//     return y ;
// }