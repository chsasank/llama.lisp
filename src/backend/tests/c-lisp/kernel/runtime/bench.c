#include <stdio.h>
#include <stdlib.h>
#include <time.h>

float flops(int m, int n, int k, float time) {
    return 2 * (m * n * k) / time / 1e9;
}

clock_t timer() {
    return clock(); 
}
