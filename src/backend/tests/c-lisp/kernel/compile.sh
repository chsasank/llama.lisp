#!/bin/bash

if [ $# -eq 0 ]; then
    echo "No input arguments provided."
    exit 1
fi

build_dir=build

if [ ! -d "$build_dir" ]; then
    echo "Creating build directory..."
    mkdir "$build_dir"
fi

for input in "$@"; do
    input_file="$input.sexp"
    kernel_object="$input.o"
    ir_file="$input.ll"
    executable_file="$input"

    if [ ! -e $input_file ]; then
        echo "File '$input_file' not found."
        exit 1
    fi

    guile ../../../utils/sexp-json.scm < $input_file \
      | python ../../../c-lisp.py \
      | python ../../../brilisp.py \
      | python ../../../llvm.py > build/$ir_file 

    clang -O1 -Wno-implicit-function-declaration -c -o build/$kernel_object build/$ir_file
done

clang -O1 -Wno-implicit-function-declaration -c -o build/main.o runtime/main.c
clang -O1 -Wno-implicit-function-declaration -c -o build/matrix.o runtime/matrix.c

objects="build/main.o build/matrix.o"
for input in "$@"; do
    kernel_object="$input.o"
    objects="$objects build/$kernel_object"
done

clang -O1 -Wno-implicit-function-declaration -o build/kernel_bench $objects

if [ $? -eq 0 ]; then
    echo "Build successful. Executable created: build/output_executable"
else
    echo "Build failed."
    exit 1
fi
