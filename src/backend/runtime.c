#include <stdio.h>

int print(int x){
    printf("%d\n", x);
    return x;
}

double fprint(double x) {
    printf("%.17f\n", x);
    return x;
}
