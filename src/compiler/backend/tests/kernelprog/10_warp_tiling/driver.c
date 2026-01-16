#include <cuda.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>  
#include <errno.h>   


#define CU_CHECK(err) if(err != CUDA_SUCCESS) { \
    const char* msg; cuGetErrorString(err,&msg); \
    fprintf(stderr,"CUDA ERROR: %s\n", msg); exit(1); }

#define CEIL_DIV(a,b) (((a) + (b) - 1) / (b))

void randomize_matrix(float* mat, int N) {
    for (int i = 0; i < N; i++) {
        mat[i] = (float)(rand() % 5) + 0.01f * (rand() % 5);
        if (rand()%2) mat[i] = -mat[i];
    }
}

void ref_kernel(int M,int N,int K,float alpha,const float* A,const float* B,float beta,float* C){
    for(int i=0;i<M;i++)
        for(int j=0;j<N;j++){
            float val=0.0f;
            for(int t=0;t<K;t++) {
                val+=A[i*K+t]*B[t*N+j];
            }
            C[i*N+j] = alpha*val + beta*C[i*N+j];
        }
}
 

int main(int argc, char** argv) {
    
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ptx_filename>\n", argv[0]);
        return 1;
    }
     
    const char* ptx_filename = argv[1];

    srand(time(NULL));

    int M=4096,N=4096,K=4096;
    float alpha=0.5f,beta=1.0f;

    float *A = malloc(sizeof(float)*M*K);
    float *B = malloc(sizeof(float)*K*N);
    float *C = malloc(sizeof(float)*M*N);
    float *C_ref = malloc(sizeof(float)*M*N);

    randomize_matrix(A,M*K);
    randomize_matrix(B,K*N);
    randomize_matrix(C,M*N);

    for(int i=0;i<M*N;i++) C_ref[i]=C[i];

    CUdevice device;
    CUcontext context;
    CUmodule module;
    CUfunction kernel;

    CU_CHECK(cuInit(0));
    CU_CHECK(cuDeviceGet(&device,0));
    CU_CHECK(cuCtxCreate(&context,0,device));

     
    FILE* f = fopen(ptx_filename,"rb");
    if (f == NULL) {
        fprintf(stderr, "Error opening PTX file %s: %s\n", ptx_filename, strerror(errno));
        return 1;
    }
    fseek(f,0,SEEK_END);
    long size = ftell(f);
    fseek(f,0,SEEK_SET);
    char* ptx = malloc(size+1);
    fread(ptx,1,size,f);
    fclose(f);
    ptx[size]=0;

    CU_CHECK(cuModuleLoadData(&module,ptx));
    free(ptx);
    CU_CHECK(cuModuleGetFunction(&kernel,module,"kernel"));  

    CUdeviceptr dA,dB,dC;
    CU_CHECK(cuMemAlloc(&dA,sizeof(float)*M*K));
    CU_CHECK(cuMemAlloc(&dB,sizeof(float)*K*N));
    CU_CHECK(cuMemAlloc(&dC,sizeof(float)*M*N));

    CU_CHECK(cuMemcpyHtoD(dA,A,sizeof(float)*M*K));
    CU_CHECK(cuMemcpyHtoD(dB,B,sizeof(float)*K*N));
    CU_CHECK(cuMemcpyHtoD(dC,C,sizeof(float)*M*N));

    
    void* args[] = { &M,&N,&K,&alpha,&dA,&dB,&beta,&dC }; 

    int blockX = 32, blockY = 32, BLOCKSIZE = 32;
    int gridX = CEIL_DIV(M, blockX);  
    int gridY = CEIL_DIV(N, blockY);  

    CUevent start, stop;
    CU_CHECK(cuEventCreate(&start, CU_EVENT_DEFAULT));
    CU_CHECK(cuEventCreate(&stop, CU_EVENT_DEFAULT));

    CU_CHECK(cuEventRecord(start, 0));

    CU_CHECK(cuLaunchKernel(kernel,
         gridX, gridY, 1,     
         BLOCKSIZE * BLOCKSIZE, 1, 1,   ////  blockX, blockY, 1,  => 1D with same no of threads
         0, 0, args, 0));     

    CU_CHECK(cuEventRecord(stop, 0));

    CU_CHECK(cuCtxSynchronize());

    float milliseconds = 0;
    CU_CHECK(cuEventElapsedTime(&milliseconds, start, stop));

    double seconds = milliseconds / 1000.0;
    double flops = 2.0 * (double)M * (double)N * (double)K;
    double tflops = (flops / seconds) / 1e12;

    printf("Kernel Time: %.3f ms\n", milliseconds);
    printf("Performance: %.4f TFLOPS\n", tflops);

    cuEventDestroy(start);
    cuEventDestroy(stop);

    CU_CHECK(cuMemcpyDtoH(C,dC,sizeof(float)*M*N));

    printf("%s", "finished GPU\n");

    // CPU reference
    ref_kernel(M,N,K,alpha,A,B,beta,C_ref);

    printf("%s", "finished CPU\n");

    // Verify
    int correct=1;
    for(int i=0;i<M*N;i++){
        if(fabs(C_ref[i]-C[i])>1e-2){ correct=0; break; }
    }

    printf("%s", "finished checking\n");

    printf("SGEMM Shared Memory %s\n", correct?"PASSED":"FAILED");

    // Cleanup
    free(A); free(B); free(C); free(C_ref);
    CU_CHECK(cuMemFree(dA));
    CU_CHECK(cuMemFree(dB));
    CU_CHECK(cuMemFree(dC));
    CU_CHECK(cuModuleUnload(module));
    CU_CHECK(cuCtxDestroy(context));

    return 0;
}
