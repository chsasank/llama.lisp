#!/bin/bash

if [ -z "$1" ]; then
    echo "No input argument provided."
    exit 1
fi

build_dir=build
input_file="$1.sexp"
kernel_object="$1.o"
ir_file="$1.ll"
executable_file="$1"

if [ ! -e $input_file ]; then
    echo "File '$input_file' not found."
    exit 1
fi

if [ ! -d "$build_dir" ]; then
    echo "Creating build directory..."
    mkdir "$build_dir"
fi

guile ../../../utils/sexp-json.scm < $input_file \
  | python ../../../c-lisp.py \
  | python ../../../brilisp.py \
  | python ../../../llvm.py > build/$ir_file 

clang -O1 -c -o build/$kernel_object build/$ir_file
clang -O1 -c -o build/main.o runtime/main.c
clang -O1 -c -o build/matrix.o runtime/matrix.c
clang -O1 -o build/$executable_file build/*.o


./build/$executable_file


