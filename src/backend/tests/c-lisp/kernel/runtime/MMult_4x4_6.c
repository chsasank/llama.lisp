void add_dot4x4(int k, float *a, int lda, float *b, int ldb, float *c, int ldc) {
    int p, i, j;

    for (p = 0; p < k; ++p) {
        for (i = 0; i < 4; ++i) {
            for (j = 0; j < 4; ++j) {
                c[j + i * ldc] += a[p + i * lda] * b[j + p * ldb];
            }
        }
    }
}

void kernel(float *a, float *b, float *c, int m, int n, int k) {
    int i, j, lda, ldb, ldc;

    lda = k;
    ldb = n;
    ldc = n;

    // Initialize matrix c to zero
    for (i = 0; i < m; ++i) {
        for (j = 0; j < n; ++j) {
            c[j + i * ldc] = 0.0f;
        }
    }

    for (j = 0; j < n; j += 4) {
        for (i = 0; i < m; i += 4) {
            add_dot4x4(k, &a[i * lda], lda, &b[j], ldb, &c[j + i * ldc], ldc);
        }
    }
}