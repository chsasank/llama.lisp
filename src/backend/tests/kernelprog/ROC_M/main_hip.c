#include<stdio.h>
#include <hip/hiprtc.h>
#include<stddef.h>
#include <hip/hip_runtime.h>
#include <string.h>
#include<stdlib.h>

#define HIP_CHECK(condition)                                                             \
{                                                                                        \
    const hipError_t error = condition;                                                  \
    if (error != hipSuccess)                                                             \
    {                                                                                    \
        fprintf(stderr, "An error encountered: \"%s\" at %s:%d\n",                       \
                hipGetErrorString(error), __FILE__, __LINE__);                           \
        exit(EXIT_FAILURE);                                                              \
    }                                                                                    \
}

#define CHECK_RET_CODE(call, ret_code)                                                             \
  {                                                                                                \
    if ((call) != ret_code) {                                                                      \
      printf("Failed in call: %s", #call ) ;                                                                                                                \
    }                                                                                              \
  }
  #define HIPRTC_CHECK(call) CHECK_RET_CODE(call, HIPRTC_SUCCESS)

 static char * arch_name(hipDeviceProp_t props){    
    if(props.gcnArchName[0])
    {
         static char *new_str = NULL ;
        if((new_str = (char *)malloc(strlen("--gpu-architecture=") + strlen(props.gcnArchName) + 1)) != NULL){
            new_str[0] = '\0';
            strcat(new_str, "--gpu-architecture=");
            strcat(new_str, props.gcnArchName);
            
            return new_str ; 
        }

        else{
            printf( "malloc failed") ;
            return new_str; 
        }
        
    }

}

char str[] = " extern \"C\"  __global__ void saxpy_kernel(const float a, const float * d_x, float * d_y, const unsigned int size)\n"
"{\n"
"    const unsigned int global_idx = blockIdx.x * blockDim.x + threadIdx.x;\n"
"    if(global_idx < size)\n"
"    {\n"
"        d_y[global_idx] = a * d_x[global_idx] + d_y[global_idx];\n"
"    }\n"
"}";

int ceiling_div(int size, int block_size){
    // return (size/block_size) ;
    return (size + block_size - 1) / block_size;
}

int main(){
   // program to be compiled in runtime.
    hiprtcProgram prog;

    // create program.
     char * const_str = str ;
  hiprtcResult result = hiprtcCreateProgram(&prog,
                        const_str,
                        "saxpy_kernel.c",
                        0,
                        NULL,
                        NULL
                        );
    if(result!= HIPRTC_SUCCESS){
        printf("FAILED. hiprtcCreateProgram");
    }

    printf("created program") ;

    // Get device properties from the first device available.
    hipDeviceProp_t props;
    unsigned int device_id = 0;
    HIP_CHECK(hipGetDeviceProperties(&props, device_id)); 
    const char * arch = arch_name(props) ;
    printf("\n%s",arch);
    result = hiprtcCompileProgram(prog,1, &arch )  ;
    // free(arch);
    if(result!= HIPRTC_SUCCESS){
        printf("FAILED hiprtcCompileProgram.");
    }

    // Get the size of the log (possibly) generated during compilation.
    size_t log_size;
    HIPRTC_CHECK( hiprtcGetProgramLogSize(prog, &log_size));

    if(log_size){
        char log[log_size];
        for(int i = 0; i<log_size; i++ ){
            log[i] = '\0';
        }
        hiprtcGetProgramLog(prog, &log[0]);
        printf("this is log %s\n",log);    
    }
    size_t code_size;
    HIPRTC_CHECK(hiprtcGetCodeSize(prog, &code_size));
    
    //store the compiled binary ; in a character array 
    char code[code_size] ;
    memset(code, '\0', code_size);
    HIPRTC_CHECK(hiprtcGetCode(prog, code));

    //Destroy program object.
    HIPRTC_CHECK(hiprtcDestroyProgram(&prog));

    //Now we launch the kernel on the device.

    //total number of float elements in each device vector;
    unsigned int size = 4096;

    //total number of bytes to allocate for each device vector;
    size_t size_bytes = size * sizeof(float);

    //Number of threads per kernel block.
    unsigned int block_size = 128 ;

    //number of blocks per kernel grid, calculated as ceil (size/block_size)
    unsigned int grid_size = ceiling_div(size, block_size);
    printf("\n%u\n\n",grid_size) ;

    //constant value "a" to be used in the expression sequence 1,2,3,4.....


    float x[size];
    float y[size];
    memset(x, 0.0f, size);

    for(int i = 0; i<size; i++){
             float j =  i ;
            x[i] = j + 1.f ;
        }

    memset(y, 0.0f, size);
    for(int i = 0; i<size; i++){
            y[i] = i + 1.f ;
            y[i] = y[i]*2;
        }

    // Allocate vectors in device and copy from host to device memory.
    float * d_x ;
    float * d_y ;
    HIP_CHECK(hipMalloc(&d_x, size_bytes));
    HIP_CHECK(hipMalloc(&d_y, size_bytes));
    HIP_CHECK(hipMemcpy(d_x, x, size_bytes, hipMemcpyHostToDevice ));
    HIP_CHECK(hipMemcpy(d_y, y, size_bytes, hipMemcpyHostToDevice));
    
    hipModule_t module;
    hipModuleLoadData(&module, code) ;


    // Load the HIP module corresponding to the compiled binary into the current context.
    hipFunction_t kernel;
    HIP_CHECK(hipModuleGetFunction(&kernel, module, "saxpy_kernel"));

    //create and fill array with kernel arguments.
    const float a = 5.1f;
    size_t offset = 0;
    char args[256] ;

    *((float *) (&args[offset])) = a;
    offset += sizeof(a);
    offset += 4; // aligning fix for CUDA executions

    *((float **) (&args[offset])) = d_x ; 
    offset += sizeof(d_x);
    *((float **) (&args[offset])) = d_y ;
    offset += sizeof(d_y);
    *((uint *) (&args[offset])) = size ; 
    offset += sizeof(size);

        // Create array with kernel arguments and its size.
    void* config[] = {HIP_LAUNCH_PARAM_BUFFER_POINTER,
                      args,
                      HIP_LAUNCH_PARAM_BUFFER_SIZE,
                      &offset,
                      HIP_LAUNCH_PARAM_END};
    
    printf("Calculating y[i] = a * x[i] + y[i] over %d\n", size) ;
    //HIP_CHECK(hipModuleLaunchKernel(kernel,
    HIP_CHECK(hipModuleLaunchKernel(kernel,
                                    grid_size,
                                    1,
                                    1,
                                    block_size,
                                    1,
                                    1,
                                    0,
                                    NULL,
                                    NULL,
                                    (void**)&config));

    // Check if the kernel launch was successful.
    HIP_CHECK(hipGetLastError());

    // Copy results from device to host.
    //HIP_CHECK(hipMemcpy(y.data(), d_y, size_bytes, hipMemcpyDeviceToHost));
    hipMemcpy(y, d_y, size_bytes, hipMemcpyDeviceToHost);

    // Free device memory.
    HIP_CHECK(hipFree(d_y));
    HIP_CHECK(hipFree(d_x)) ;
    // Unload module.
    HIP_CHECK(hipModuleUnload(module));
    // Print the first few elements of the results for validation.
    size_t elements_to_print = 10;
    for (int i = 0; i < 10; i++){
        printf("%f\n",y[i]) ;
    }

    return 0;
    }
      

