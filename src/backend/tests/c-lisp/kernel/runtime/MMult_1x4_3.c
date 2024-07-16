void add_dot1x4(int k, float* a, int lda, float* b, int ldb, float* c, int ldc) {
    add_dot(k, a, lda, &b[0 * ldb], &c[0 * ldc]);
    add_dot(k, a, lda, &b[1 * ldb], &c[1 * ldc]);
    add_dot(k, a, lda, &b[2 * ldb], &c[2 * ldc]);
    add_dot(k, a, lda, &b[3 * ldb], &c[3 * ldc]);
}

void kernel(float* a, float* b, float* c, int m, int n, int k) {
    
    int lda = m;
    int ldb = n;
    int ldc = k;

    for(int j = 0; j < n; j += 4) 
        for(int i = 0; i < m; i++)
            add_dot1x4(
                k, 
                &a[i],
                lda,
                &b[j * ldb],
                ldb,
                &c[j * ldc + i],
                ldc
            );

}