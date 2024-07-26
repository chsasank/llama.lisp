void add_dot1x4(int k, float* a, int lda, float* b, int ldb, float* c, int ldc) {
    c[0]       = 0.;
    c[1 * ldc] = 0.;
    c[2 * ldc] = 0.;
    c[3 * ldc] = 0.;

    int p;

    for(p = 0; p < k; p++)
        c[0 + 0 * ldc] += a[0 + p * lda] * b[p + 0 * ldb];

    for(p = 0; p < k; p++)
        c[0 + 1 * ldc] += a[0 + p * lda] * b[p + 1 * ldb];

    for(p = 0; p < k; p++)
        c[0 + 2 * ldc] += a[0 + p * lda] * b[p + 2 * ldb];

    for(p = 0; p < k; p++)
        c[0 + 3 * ldc] += a[0 + p * lda] * b[p + 3 * ldb];

}       

void kernel(float* a, float* b, float*c, int m, int n, int k) {
    
    int lda = m;
    int ldb = n;
    int ldc = k;
    
    for(int j = 0; j < n; j += 4) 
        for(int i = 0; i < m; i++) 
            add_dot1x4(k, &a[i], lda, &b[j * ldb], ldb, &c[j * ldc + i], ldc);
}