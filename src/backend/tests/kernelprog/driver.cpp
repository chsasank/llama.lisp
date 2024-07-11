#include <iostream>
#include <fstream>
#include <cassert>
#include "cuda.h"
#include <cmath>

void checkCudaErrors(CUresult err) {
  assert(err == CUDA_SUCCESS);
}

void ref_matmul (float * a, float * b, float * c, int N) {
  for (int i=0; i<N*N; i++)
    c[i] = 0.0;

  for (int i=0; i<N; i++)
    for (int j=0; j<N; j++)
      for (int k=0; k<N; k++)
        c[i*N + j] += a[i*N + k] * b[k*N + j];
}

/// main - Program entry point
int main(int argc, char **argv) {
  CUdevice    device;
  CUmodule    cudaModule;
  CUcontext   context;
  CUfunction  function;
  CUlinkState linker;
  int         devCount;

  // CUDA initialization
  checkCudaErrors(cuInit(0));
  checkCudaErrors(cuDeviceGetCount(&devCount));
  checkCudaErrors(cuDeviceGet(&device, 0));

  char name[128];
  checkCudaErrors(cuDeviceGetName(name, 128, device));
  //std::cout << "Using CUDA Device [0]: " << name << "\n";

  int devMajor, devMinor;
  checkCudaErrors(cuDeviceComputeCapability(&devMajor, &devMinor, device));
  /*std::cout << "Device Compute Capability: "
    << devMajor << "." << devMinor << "\n";*/
  if (devMajor < 2) {
    std::cerr << "ERROR: Device 0 is not SM 2.0 or greater\n";
    return 1;
  }

  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << "<filename>.ptx\n";
    return 1;
  }

  std::ifstream t(argv[1]);
  if (!t.is_open()) {
    std::cerr << argv[1] << " not found\n";
    return 1;
  }
  std::string str((std::istreambuf_iterator<char>(t)),
                    std::istreambuf_iterator<char>());

  // Create driver context
  checkCudaErrors(cuCtxCreate(&context, 0, device));

  // Create module for object
  checkCudaErrors(cuModuleLoadDataEx(&cudaModule, str.c_str(), 0, 0, 0));

  // Get kernel function
  checkCudaErrors(cuModuleGetFunction(&function, cudaModule, "kernel"));

  // Input size
  int N = 1024;
  int BlockDim = 32;
  int GridDim = (N + BlockDim - 1) / BlockDim;

  // Device data
  CUdeviceptr devBufferA;
  CUdeviceptr devBufferB;
  CUdeviceptr devBufferC;

  checkCudaErrors(cuMemAlloc(&devBufferA, sizeof(float)*N*N));
  checkCudaErrors(cuMemAlloc(&devBufferB, sizeof(float)*N*N));
  checkCudaErrors(cuMemAlloc(&devBufferC, sizeof(float)*N*N));


  float * hostA = (float *) std::malloc(sizeof(float) * N * N);
  float * hostB = (float *) std::malloc(sizeof(float) * N * N);
  float * hostC = (float *) std::malloc(sizeof(float) * N * N);
  float * hostRes = (float *) std::malloc(sizeof(float) * N * N);

  // Populate input
  for (unsigned i = 0; i != N*N; ++i) {
    hostA[i] = (float)i;
    hostB[i] = (float)(2*i);
    hostC[i] = 0.0f;
  }

  checkCudaErrors(cuMemcpyHtoD(devBufferA, &hostA[0], sizeof(float)*N*N));
  checkCudaErrors(cuMemcpyHtoD(devBufferB, &hostB[0], sizeof(float)*N*N));


  unsigned blockSizeX = BlockDim;
  unsigned blockSizeY = BlockDim;
  unsigned blockSizeZ = 1;
  unsigned gridSizeX  = GridDim;
  unsigned gridSizeY  = GridDim;
  unsigned gridSizeZ  = 1;

  // Kernel parameters
  void *KernelParams[] = { &devBufferA, &devBufferB, &devBufferC, &N };

  std::cout << "Launching kernel\n";

  // Kernel launch
  checkCudaErrors(cuLaunchKernel(function, gridSizeX, gridSizeY, gridSizeZ,
                                 blockSizeX, blockSizeY, blockSizeZ,
                                 0, NULL, KernelParams, NULL));
  cuCtxSynchronize();

  // Retrieve device data
  checkCudaErrors(cuMemcpyDtoH(&hostC[0], devBufferC, sizeof(float)*N*N));

  std::cout << "Computing reference\n";
  ref_matmul(hostA, hostB, hostRes, N);

  //std::cout << "Max error:\n";
  //for (unsigned i = 0; i != 16; ++i) {
  //  std::cout << hostA[i] << " + " << hostB[i] << " = " << hostC[i] << "\n";
  //}
  float max_err = 0.0;
  float sum = 0.0;
  for (int i=0; i<N*N; i++) {
    max_err = fmax(abs(hostC[i] - hostRes[i]), max_err);
    sum += hostC[i];
  }

  std::cout << "Max error: " << max_err << "\n";
  std::cout << "Sum: " << sum << "\n";

  // Clean up after ourselves
  delete [] hostA;
  delete [] hostB;
  delete [] hostC;

  // Clean-up
  checkCudaErrors(cuMemFree(devBufferA));
  checkCudaErrors(cuMemFree(devBufferB));
  checkCudaErrors(cuMemFree(devBufferC));
  checkCudaErrors(cuModuleUnload(cudaModule));
  checkCudaErrors(cuCtxDestroy(context));

  return 0;
}
