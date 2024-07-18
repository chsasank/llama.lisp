void add_dot4x4(int k, float* a, int lda, float* b, int ldb, float* c, int ldc) {

    // First row
    c[0 + 0 * ldc] = 0.;
    c[0 + 1 * ldc] = 0.;
    c[0 + 2 * ldc] = 0.;
    c[0 + 3 * ldc] = 0.;

    for(int p = 0; p < k; p++)
        c[0 + 0 * ldc] += a[0 + p * lda] * b[p + 0 * ldb]; 
    for(int p = 0; p < k; p++)
        c[0 + 1 * ldc] += a[0 + p * lda] * b[p + 1 * ldb]; 
    for(int p = 0; p < k; p++)
        c[0 + 2 * ldc] += a[0 + p * lda] * b[p + 2 * ldb]; 
    for(int p = 0; p < k; p++)
        c[0 + 3 * ldc] += a[0 + p * lda] * b[p + 2 * ldb]; 

    // Second row
    c[1 + 0 * ldc] = 0.;
    c[1 + 1 * ldc] = 0.;
    c[1 + 2 * ldc] = 0.;
    c[1 + 3 * ldc] = 0.;

    for(int p = 0; p < k; p++)
        c[1 + 0 * ldc] += a[1 + p * lda] * b[p + 0 * ldb]; 
    for(int p = 0; p < k; p++)
        c[1 + 1 * ldc] += a[1 + p * lda] * b[p + 1 * ldb]; 
    for(int p = 0; p < k; p++)
        c[1 + 2 * ldc] += a[1 + p * lda] * b[p + 2 * ldb]; 
    for(int p = 0; p < k; p++)
        c[1 + 3 * ldc] += a[1 + p * lda] * b[p + 2 * ldb]; 

    // Third row
    c[2 + 0 * ldc] = 0.;
    c[2 + 1 * ldc] = 0.;
    c[2 + 2 * ldc] = 0.;
    c[2 + 3 * ldc] = 0.;

    for(int p = 0; p < k; p++)
        c[2 + 0 * ldc] += a[2 + p * lda] * b[p + 0 * ldb]; 
    for(int p = 0; p < k; p++)
        c[2 + 1 * ldc] += a[2 + p * lda] * b[p + 1 * ldb]; 
    for(int p = 0; p < k; p++)
        c[2 + 2 * ldc] += a[2 + p * lda] * b[p + 2 * ldb]; 
    for(int p = 0; p < k; p++)
        c[2 + 3 * ldc] += a[2 + p * lda] * b[p + 2 * ldb]; 

    // Fourth row
    c[3 + 0 * ldc] = 0.;
    c[3 + 1 * ldc] = 0.;
    c[3 + 2 * ldc] = 0.;
    c[3 + 3 * ldc] = 0.;

    for(int p = 0; p < k; p++)
        c[3 + 0 * ldc] += a[3 + p * lda] * b[p + 0 * ldb]; 
    for(int p = 0; p < k; p++)
        c[3 + 1 * ldc] += a[3 + p * lda] * b[p + 1 * ldb]; 
    for(int p = 0; p < k; p++)
        c[3 + 2 * ldc] += a[3 + p * lda] * b[p + 2 * ldb]; 
    for(int p = 0; p < k; p++)
        c[3 + 3 * ldc] += a[3 + p * lda] * b[p + 2 * ldb]; 
}

void kernel(float* a, float* b, float* c, int m, int n, int k) {
    int lda = m;
    int ldb = n;
    int ldc = k;

    for(int j = 0; j < n; j += 4)
        for(int i = 0; i < m; i += 4)
            add_dot4x4(k, &a[i + 0 * lda], lda, &b[0 + j * ldb], ldb, &c[i + j * ldc], ldc);
}