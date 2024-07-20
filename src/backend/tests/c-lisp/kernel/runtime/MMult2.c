#include "add_dot.c"

void kernel(float* a, float* b, float* c, int m, int n, int k){
    
    int lda = m;
    int ldb = n;
    int ldc = k;

    for(int j = 0; j < n; j += 4) {
        for(int i = 0; i < m; i++) {
            add_dot(k, &a[0 * lda + i], lda, &b[(j + 0) * ldb], &c[(j + 0) * ldc + i]);
            add_dot(k, &a[0 * lda + i], lda, &b[(j + 1) * ldb], &c[(j + 1) * ldc + i]);
            add_dot(k, &a[0 * lda + i], lda, &b[(j + 2) * ldb], &c[(j + 2) * ldc + i]);
            add_dot(k, &a[0 * lda + i], lda, &b[(j + 3) * ldb], &c[(j + 3) * ldc + i]);
        }
    }
}