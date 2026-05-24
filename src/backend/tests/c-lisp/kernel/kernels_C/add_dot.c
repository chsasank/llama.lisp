void add_dot(int k, float* a, int incx ,float* b, float* c) {
    float a_value, b_value;
    float acc = 0.;

    for(int p = 0; p < k; p++) {
        a_value = a[p * incx];
        b_value = b[p];
        acc += a_value * b_value;
    }
    c[0] = acc;
}