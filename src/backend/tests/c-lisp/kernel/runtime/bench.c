#include <stdio.h>
#include <stdlib.h>
#include <time.h>

double flops(int m, int n, int k, double time) {
    double num_ops = 2 * (m * n * k);
    return num_ops / (time * 1e9);
}

clock_t timer() {
    return clock(); 
}
