void add_dot4x4(int k, float* a, int lda, float* b, int ldb, float* c, int ldc) {
    c[0 + 0 * ldc] = 0.0f;
    c[0 + 1 * ldc] = 0.0f;
    c[0 + 2 * ldc] = 0.0f;
    c[0 + 3 * ldc] = 0.0f;

    c[1 + 0 * ldc] = 0.0f;
    c[1 + 1 * ldc] = 0.0f;
    c[1 + 2 * ldc] = 0.0f;
    c[1 + 3 * ldc] = 0.0f;

    c[2 + 0 * ldc] = 0.0f;
    c[2 + 1 * ldc] = 0.0f;
    c[2 + 2 * ldc] = 0.0f;
    c[2 + 3 * ldc] = 0.0f;

    c[3 + 0 * ldc] = 0.0f;
    c[3 + 1 * ldc] = 0.0f;
    c[3 + 2 * ldc] = 0.0f;
    c[3 + 3 * ldc] = 0.0f;

    for (int p = 0; p < k; ++p) {
        c[0 + 0 * ldc] += a[0 + p * lda] * b[p + 0 * ldb];
        c[0 + 1 * ldc] += a[0 + p * lda] * b[p + 1 * ldb];
        c[0 + 2 * ldc] += a[0 + p * lda] * b[p + 2 * ldb];
        c[0 + 3 * ldc] += a[0 + p * lda] * b[p + 3 * ldb];

        c[1 + 0 * ldc] += a[1 + p * lda] * b[p + 0 * ldb];
        c[1 + 1 * ldc] += a[1 + p * lda] * b[p + 1 * ldb];
        c[1 + 2 * ldc] += a[1 + p * lda] * b[p + 2 * ldb];
        c[1 + 3 * ldc] += a[1 + p * lda] * b[p + 3 * ldb];

        c[2 + 0 * ldc] += a[2 + p * lda] * b[p + 0 * ldb];
        c[2 + 1 * ldc] += a[2 + p * lda] * b[p + 1 * ldb];
        c[2 + 2 * ldc] += a[2 + p * lda] * b[p + 2 * ldb];
        c[2 + 3 * ldc] += a[2 + p * lda] * b[p + 3 * ldb];

        c[3 + 0 * ldc] += a[3 + p * lda] * b[p + 0 * ldb];
        c[3 + 1 * ldc] += a[3 + p * lda] * b[p + 1 * ldb];
        c[3 + 2 * ldc] += a[3 + p * lda] * b[p + 2 * ldb];
        c[3 + 3 * ldc] += a[3 + p * lda] * b[p + 3 * ldb];
    }
}

void kernel(float* a, float* b, float* c, int m, int n, int k) {
    int lda = m;
    int ldb = n;
    int ldc = k;

    for (int j = 0; j < n; j += 4) 
        for (int i = 0; i < m; i += 4) 
            add_dot4x4(k, &a[i], lda, &b[j * ldb], ldb, &c[i + j * ldc], ldc);
}
