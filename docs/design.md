# Development Process

We will follow an iterative and incremental process to the development of the language.

## Goals

To tame the dragon that is complexity of compiler development, I will start with a simple goal:

> Generate matrix multiplication kernel that is as performant as hand-optimized version on a single threaded CPU.

This goal is already challenging enough and is basis of many libraries like BLAS[1], ATLAS[2] and so on.

For the first version, I aim to create an interpreter instead of compiler to get the basics of interface right. Once I create an interpreted version of matrix multiplication, I will start working on emitting LLVM/x86 code that is as good as code generated by naive C kernel.

I will follow this amazing tutorial[3] from [here](https://github.com/flame/how-to-optimize-gemm/wiki) to generate code for each of the steps mentioned here. Each step should add interesting features to my code base which makes it incrementtal, interesting and engaging.

## Documentation

Any software lives or dies by its documentation. Not only will the code be documented but the whole development process will be documented. The code developed here should have an education value. Each design decision and development step should be documented as a markdown file in `docs` directory.


References:
1. BLAS TODO
2. ATLAS TODO
3. BLIS TODO