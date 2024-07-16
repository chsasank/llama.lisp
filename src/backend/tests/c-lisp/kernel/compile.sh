#!/bin/bash

if [ $# -eq 0 ]; then
    echo "No input arguments provided."
    exit 1
fi

build_dir=build
optimization=-O1

if [ ! -d "$build_dir" ]; then
    echo "Creating build directory..."
    mkdir "$build_dir"
    mkdir "$build_dir/object"
    mkdir "$build_dir/ir"
fi

for input in "$@"; do
    input_file="$input.sexp"
    kernel_object="$input.clisp.o"
    ir_file="$input.ll"
    executable_file="$input"

    if [ ! -e $input_file ]; then
        echo "File '$input_file' not found."
        exit 1
    fi

    guile ../../../utils/sexp-json.scm < $input_file \
      | python ../../../c-lisp.py \
      | python ../../../brilisp.py \
      | python ../../../llvm.py > build/ir/$ir_file 

    clang $optimization -Wno-implicit-function-declaration -c -o build/object/$kernel_object build/ir/$ir_file
done

for input in "$@"; do
    c_file="$input.c"
    obj_file="$input.c.o"

    clang $optimization -Wno-implicit-function-declaration -c -o build/object/$obj_file runtime/$c_file
done

clang $optimization -Wno-implicit-function-declaration -c -o build/object/main.o runtime/main.c
clang $optimization -Wno-implicit-function-declaration -c -o build/object/matrix.o runtime/matrix.c

objects="build/object/main.o build/object/matrix.o"
for input in "$@"; do
    c_kernel_object="$input.c.o"
    clisp_kernel_object="$input.clisp.o"
    objects="$objects build/object/$c_kernel_object"
    objects="$objects build/object/$clisp_kernel_object"
done

clang $optimization -Wno-implicit-function-declaration -o build/kernel_bench $objects
