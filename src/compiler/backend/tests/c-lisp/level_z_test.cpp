// spv_runner_l0.cpp
#include <level_zero/ze_api.h>
#include <fstream>
#include <vector>
#include <iostream>
#include <string>
#include <iomanip>

#define CHECK(call) do { \
    ze_result_t res = call; \
    if (res != ZE_RESULT_SUCCESS) { \
        std::cerr << "L0 Error " << res << " at line " << __LINE__ << std::endl; \
        std::exit(1); \
    } \
} while(0)

std::vector<uint8_t> readSPV(const std::string& path) {
    std::ifstream f(path, std::ios::binary | std::ios::ate);
    if (!f) { std::cerr << "Cannot open " << path << std::endl; std::exit(1); }
    size_t sz = f.tellg();
    f.seekg(0);
    std::vector<uint8_t> data(sz);
    f.read(reinterpret_cast<char*>(data.data()), sz);
    return data;
}

int main(int argc, char* argv[]) {
    std::string spvPath = (argc > 1) ? argv[1] : "/home/johnaic-admin/intel_GPU_tests/llama.lisp/src/compiler/backend/tests/c-lisp/vecadd_intel.spv";
    uint32_t N = (argc > 2) ? std::stoul(argv[2]) : 10000;

    CHECK(zeInit(0));

    uint32_t drvCnt = 1; ze_driver_handle_t driver;
    CHECK(zeDriverGet(&drvCnt, &driver));

    uint32_t devCnt = 1; ze_device_handle_t device;
    CHECK(zeDeviceGet(driver, &devCnt, &device));

    ze_context_desc_t ctxDesc = {ZE_STRUCTURE_TYPE_CONTEXT_DESC};
    ze_context_handle_t context;
    CHECK(zeContextCreate(driver, &ctxDesc, &context));

    auto spv = readSPV(spvPath);
    std::cout << "Loaded SPIR-V: " << spv.size() << " bytes\n";

    // Module
    ze_module_desc_t modDesc = {ZE_STRUCTURE_TYPE_MODULE_DESC};
    modDesc.format = ZE_MODULE_FORMAT_IL_SPIRV;
    modDesc.inputSize = spv.size();
    modDesc.pInputModule = spv.data();
    modDesc.pBuildFlags = "-ze-opt-level=2";

    ze_module_handle_t module;
    CHECK(zeModuleCreate(context, device, &modDesc, &module, nullptr));

    // Kernel
    ze_kernel_desc_t kernDesc = {ZE_STRUCTURE_TYPE_KERNEL_DESC};
    kernDesc.pKernelName = "kernel";

    ze_kernel_handle_t kernel;
    CHECK(zeKernelCreate(module, &kernDesc, &kernel));

    // USM
    int *A = nullptr, *B = nullptr, *C = nullptr;
    ze_device_mem_alloc_desc_t ddesc = {ZE_STRUCTURE_TYPE_DEVICE_MEM_ALLOC_DESC};
    ze_host_mem_alloc_desc_t hdesc = {ZE_STRUCTURE_TYPE_HOST_MEM_ALLOC_DESC};

    CHECK(zeMemAllocShared(context, &ddesc, &hdesc, N*sizeof(int), 64, device, (void**)&A));
    CHECK(zeMemAllocShared(context, &ddesc, &hdesc, N*sizeof(int), 64, device, (void**)&B));
    CHECK(zeMemAllocShared(context, &ddesc, &hdesc, N*sizeof(int), 64, device, (void**)&C));

    for(uint32_t i = 0; i < N; i++) {
        A[i] = i;
        B[i] = i;
    }

    CHECK(zeKernelSetArgumentValue(kernel, 0, sizeof(void*), &C));
    CHECK(zeKernelSetArgumentValue(kernel, 1, sizeof(void*), &A));
    CHECK(zeKernelSetArgumentValue(kernel, 2, sizeof(void*), &B));

    CHECK(zeKernelSetGroupSize(kernel, 256, 1, 1));
    ze_group_count_t dispatch = {(N + 255)/256, 1, 1};

    ze_command_queue_desc_t qdesc = {ZE_STRUCTURE_TYPE_COMMAND_QUEUE_DESC};
    ze_command_list_desc_t ldesc = {ZE_STRUCTURE_TYPE_COMMAND_LIST_DESC};

    ze_command_queue_handle_t queue;
    ze_command_list_handle_t list;

    CHECK(zeCommandQueueCreate(context, device, &qdesc, &queue));
    CHECK(zeCommandListCreate(context, device, &ldesc, &list));

    CHECK(zeCommandListAppendLaunchKernel(list, kernel, &dispatch, nullptr, 0, nullptr));
    CHECK(zeCommandListClose(list));
    CHECK(zeCommandQueueExecuteCommandLists(queue, 1, &list, nullptr));
    CHECK(zeCommandQueueSynchronize(queue, UINT64_MAX));

    // === Print Results Like Original SYCL Sample ===
    std::cout << "Running on device: Intel GPU (Level Zero)\n";
    std::cout << "Vector size: " << N << "\n\n";

    int indices[] = {0, 1, 2, static_cast<int>(N)-1};
    for (int idx : indices) {
        if (idx == indices[3] && N > 10) std::cout << "...\n";
        std::cout << "[" << idx << "]: " << idx << " + " << idx 
                  << " = " << C[idx] << "\n";
    }

    // Verify
    bool passed = true;
    for(uint32_t i = 0; i < N; i++) {
        if(C[i] != A[i] + B[i]) {
            passed = false;
            break;
        }
    }

    std::cout << "\nVector add successfully completed.\n";

    if (passed)
        std::cout << "Test PASSED!\n";
    else
        std::cout << "Test FAILED!\n";

    // Cleanup
    zeMemFree(context, A);
    zeMemFree(context, B);
    zeMemFree(context, C);
    zeKernelDestroy(kernel);
    zeModuleDestroy(module);
    zeCommandListDestroy(list);
    zeCommandQueueDestroy(queue);
    zeContextDestroy(context);

    return 0;
}