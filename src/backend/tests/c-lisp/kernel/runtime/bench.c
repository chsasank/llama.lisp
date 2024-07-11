#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// __attribute__((optnone))
double flops(int m, int n, int k, double time) {
    // printf("%.3f\n", time);      /** FOR DEBUGGING */
    double num_ops = 2.0 * (double)m * (double)n * (double)k;
    return num_ops / (time * 1e9);
}


clock_t timer() {
    return clock(); 
}
