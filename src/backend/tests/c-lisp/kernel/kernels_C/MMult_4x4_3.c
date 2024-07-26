#include "add_dot.c"

void add_dot4x4(int k, float* a, int lda, float* b, int ldb, float* c, int ldc) {
    
    // First row
    add_dot(k, &a[0], lda, &b[0 * ldb], &c[0 + 0 * ldc]);
    add_dot(k, &a[0], lda, &b[1 * ldb], &c[0 + 1 * ldc]);
    add_dot(k, &a[0], lda, &b[2 * ldb], &c[0 + 2 * ldc]);
    add_dot(k, &a[0], lda, &b[3 * ldb], &c[0 + 3 * ldc]);

    // Second row
    add_dot(k, &a[1], lda, &b[0 * ldb], &c[1 + 0 * ldc]);
    add_dot(k, &a[1], lda, &b[1 * ldb], &c[1 + 1 * ldc]);
    add_dot(k, &a[1], lda, &b[2 * ldb], &c[1 + 2 * ldc]);
    add_dot(k, &a[1], lda, &b[3 * ldb], &c[1 + 3 * ldc]);

    // Third row
    add_dot(k, &a[2], lda, &b[0 * ldb], &c[2 + 0 * ldc]);
    add_dot(k, &a[2], lda, &b[1 * ldb], &c[2 + 1 * ldc]);
    add_dot(k, &a[2], lda, &b[2 * ldb], &c[2 + 2 * ldc]);
    add_dot(k, &a[2], lda, &b[3 * ldb], &c[2 + 3 * ldc]);

    // Fourth row
    add_dot(k, &a[3], lda, &b[0 * ldb], &c[3 + 0 * ldc]);
    add_dot(k, &a[3], lda, &b[1 * ldb], &c[3 + 1 * ldc]);
    add_dot(k, &a[3], lda, &b[2 * ldb], &c[3 + 2 * ldc]);
    add_dot(k, &a[3], lda, &b[3 * ldb], &c[3 + 3 * ldc]);
}

void kernel(float* a, float* b, float* c, int m, int n, int k) {
    
    int lda = m;
    int ldb = n;
    int ldc = k;
    
    for(int j = 0; j < n; j += 4)
        for(int i = 0; i < m; i += 4)
            add_dot4x4(k, &a[i], lda, &b[j * ldb], ldb, &c[j * ldc + i], ldc);
}