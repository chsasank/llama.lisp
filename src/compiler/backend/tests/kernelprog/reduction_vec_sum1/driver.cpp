#include <cuda.h>
#include <iostream>
#include <vector>
#include <cassert>
#include <functional>   

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

template <class T>
float measure_performance(std::function<T(CUstream)> bound_function,
                          CUstream stream,
                          size_t num_repeats = 10,
                          size_t num_warmups = 10)
{
    CUevent start = nullptr, stop = nullptr;
    float time_ms = 0.0f;

    CHECK_CU(cuEventCreate(&start, CU_EVENT_DEFAULT));
    CHECK_CU(cuEventCreate(&stop,  CU_EVENT_DEFAULT));

    for (size_t i = 0; i < num_warmups; ++i) {
        bound_function(stream);
    }
    CHECK_CU(cuStreamSynchronize(stream));

    CHECK_CU(cuEventRecord(start, stream));
    for (size_t i = 0; i < num_repeats; ++i) {
        bound_function(stream);
    }
    CHECK_CU(cuEventRecord(stop, stream));

    CHECK_CU(cuEventSynchronize(stop));
    CHECK_CU(cuEventElapsedTime(&time_ms, start, stop));

    CHECK_CU(cuEventDestroy(start));
    CHECK_CU(cuEventDestroy(stop));

    return time_ms / static_cast<float>(num_repeats);
}


int main()
{
    constexpr size_t NUM_THREADS           = 256;
    constexpr size_t batch_size            = 2048;
    constexpr size_t num_elements_per_batch = 1024 * 256;
    constexpr size_t total_elements        = batch_size * num_elements_per_batch;

    CHECK_CU(cuInit(0));

    CUdevice   device;
    CUcontext  context;
    CUmodule   module;
    CUfunction kernel;

    CHECK_CU(cuDeviceGet(&device, 0));
    char name[256];  
    CHECK_CU(cuDeviceGetName(name, 256, device));
    std::cout << "Device Name: " << name << std::endl;
    size_t total_bytes = 0;
    CHECK_CU(cuDeviceTotalMem(&total_bytes, device));
    double mb = static_cast<double>(total_bytes) / (1024.0 * 1024.0);
    double gb = mb / 1024.0;
    std::cout << "Total Memory: " << mb << " MB (" << gb << " GB)" << std::endl;
    int memClockKHz = 0;
    int busWidthBits = 0;
    CHECK_CU(cuDeviceGetAttribute(&memClockKHz, CU_DEVICE_ATTRIBUTE_MEMORY_CLOCK_RATE, device));
    CHECK_CU(cuDeviceGetAttribute(&busWidthBits, CU_DEVICE_ATTRIBUTE_GLOBAL_MEMORY_BUS_WIDTH, device));
    float const peak_bandwidth{static_cast<float>(2.0f * memClockKHz * (busWidthBits / 8) / 1.0e6)};
    std::cout << "Peak Bandwidth: " << peak_bandwidth << " GB/s" << std::endl;
    std::cout << "Batch Size: " << batch_size << std::endl;
    std::cout << "Number of Elements Per Batch: " << num_elements_per_batch<< std::endl;

    // Kernel results verification:
    CHECK_CU(cuCtxCreate(&context, 0, device));
    CHECK_CU(cuModuleLoad(&module, "kernel.ptx"));
    CHECK_CU(cuModuleGetFunction(&kernel, module, "kernel"));

    CUdeviceptr d_input = 0, d_output = 0;
    CHECK_CU(cuMemAlloc(&d_input,  total_elements * sizeof(float)));
    CHECK_CU(cuMemAlloc(&d_output, batch_size  * sizeof(float)));

    std::vector<float> h_input(total_elements, 1.0f);
    std::vector<float> h_output(batch_size);

    CHECK_CU(cuMemcpyHtoD(d_input, h_input.data(), total_elements * sizeof(float)));

    void* args[] = { &d_output, &d_input, (void*)&num_elements_per_batch };

    CHECK_CU(cuLaunchKernel (kernel,
                batch_size, 1, 1,     // grid
                NUM_THREADS, 1, 1,    // block
                0, nullptr, args, nullptr));

    CHECK_CU(cuCtxSynchronize());

    CHECK_CU(cuMemcpyDtoH(h_output.data(), d_output, batch_size * sizeof(float)));

    for (size_t i = 0; i < batch_size; ++i) {
        assert(h_output[i] == static_cast<float>(num_elements_per_batch));
    }

    std::cout << "SUCCESS: All results correct\n";

    //Performance
    CUstream stream = nullptr;
    CHECK_CU(cuStreamCreate(&stream, CU_STREAM_NON_BLOCKING));

    auto bound_launch = [&](CUstream s) {
        CHECK_CU(cuLaunchKernel(kernel,
                                batch_size, 1, 1,
                                NUM_THREADS, 1, 1,
                                0,
                                s,           
                                args,
                                nullptr));
    };

    float avg_latency_ms = measure_performance<void>(bound_launch, stream, 50, 20);
 
    // Effective Bandwidth = (Bytes Read + Bytes Written) / Time
    size_t total_bytes_moved = (total_elements + batch_size) * sizeof(float);
    float eff_bandwidth = (total_bytes_moved * 1e-6f) / avg_latency_ms;
    std::cout << "\n--- Performance Results ---" << std::endl;
    std::cout << "Avg Latency:    " << avg_latency_ms << " ms" << std::endl;
    std::cout << "Effective BW:   " << eff_bandwidth << " GB/s" << std::endl;
    std::cout << "Efficiency:     " << (eff_bandwidth / peak_bandwidth) * 100.0f << "%" << std::endl;

    CHECK_CU(cuStreamDestroy(stream));
    CHECK_CU(cuMemFree(d_input));
    CHECK_CU(cuMemFree(d_output));
    CHECK_CU(cuModuleUnload(module));
    CHECK_CU(cuCtxDestroy(context));

    return 0;
}