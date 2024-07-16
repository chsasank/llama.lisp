void add_dot1x4(int k, float* a, int lda, float* b, int ldb, float* c, int ldc) {
    c[0]       = 0.;
    c[1 * ldc] = 0.;
    c[2 * ldc] = 0.;
    c[3 * ldc] = 0.;

    int p;

    for(p = 0; p < k; p++)
        c[0] += a[p * lda] * b[p];

    for(p = 0; p < k; p++)
        c[1 * ldc] += a[p * lda] * b[p + ldb * 1];

    for(p = 0; p < k; p++)
        c[2 * ldc] += a[p * lda] * b[p + ldb * 2];

    for(p = 0; p < k; p++)
        c[3 * ldc] += a[p * lda] * b[p + ldb * 3];

}       

void kernel(float* a, float* b, float*c, int m, int n, int k) {
    
    int lda = m;
    int ldb = n;
    int ldc = k;
    
    for(int j = 0; j < n; j += 4) 
        for(int i = 0; i < m; i++) 
            add_dot1x4(k, &a[i], lda, &b[j * ldb], ldb, &c[j * ldc + i], ldc);
}