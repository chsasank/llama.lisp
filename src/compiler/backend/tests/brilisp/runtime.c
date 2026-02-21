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

// to print the value of the pointer
int* ptr_print(int* x){
    printf("%p\n",x);
    return x ;
}

int64_t int64_print(int64_t x){
    printf("%"PRId64"\n",x);
    return x ;
}