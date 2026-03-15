#include <cuda.h>
#include <iostream>
#include <vector>
#include <cassert>

#define CHECK_CU(x)                                    \
    do {                                               \
        CUresult err = x;                              \
        if (err != CUDA_SUCCESS) {                     \
            const char* msg;                           \
            cuGetErrorString(err, &msg);               \
            std::cerr << "CUDA error: " << msg         \
                      << " at " << __LINE__ << "\n";   \
            std::exit(1);                              \
        }                                              \
    } while (0)

int main()
{
    constexpr size_t NUM_THREADS = 256;
    constexpr size_t batch_size = 2048;
    constexpr size_t num_elements_per_batch = 1024 * 256;
    constexpr size_t total_elements =
        batch_size * num_elements_per_batch;

    CHECK_CU(cuInit(0));

    CUdevice device;
    CHECK_CU(cuDeviceGet(&device, 0));

    CUcontext context;
    CHECK_CU(cuCtxCreate(&context, 0, device));

    CUmodule module;
    CHECK_CU(cuModuleLoad(&module, "kernel.ptx"));

    CUfunction kernel;
    CHECK_CU(cuModuleGetFunction(
        &kernel, module, "kernel"));

    CUdeviceptr d_input, d_output;
    CHECK_CU(cuMemAlloc(&d_input,
                        total_elements * sizeof(float)));
    CHECK_CU(cuMemAlloc(&d_output,
                        batch_size * sizeof(float)));

  
    std::vector<float> h_input(total_elements, 1.0f);
    std::vector<float> h_output(batch_size);

    CHECK_CU(cuMemcpyHtoD(d_input, h_input.data(),
                          total_elements * sizeof(float)));

    void* args[] = {
        &d_output,
        &d_input,
        (void*)&num_elements_per_batch
    };

    CHECK_CU(cuLaunchKernel(
        kernel,
        batch_size, 1, 1,        // grid
        NUM_THREADS, 1, 1,       // block
        0,                       // shared mem
        nullptr,                 // stream
        args,
        nullptr));

    CHECK_CU(cuCtxSynchronize());

    CHECK_CU(cuMemcpyDtoH(h_output.data(), d_output,
                          batch_size * sizeof(float)));
    
    for (size_t i = 0; i < batch_size; ++i){
        assert(h_output[i] == num_elements_per_batch);
    }

    std::cout << "SUCCESS: All results correct\n";

    cuMemFree(d_input);
    cuMemFree(d_output);
    cuModuleUnload(module);
    cuCtxDestroy(context);

    return 0;
}
