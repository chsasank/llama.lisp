#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// __attribute__((optnone))
double flops(int m, int n, int k, double time) {
    // printf("%.3f\n", time);      /** FOR DEBUGGING */
    double num_ops = 2.0 * (double)m * (double)n * (double)k;
    double result = num_ops / (time * 1e9);
    return isnan(result) ? 0 : result;
}

clock_t timer() {
    return clock(); 
}
