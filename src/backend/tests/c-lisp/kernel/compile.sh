#!/bin/bash

if [ -z "$1" ]; then
    echo "No input argument provided."
    exit 1
fi

input_file="$1.sexp"
kernel_object="$1.o"
ir_file="$1.ll"
executable_file="$1.out"

guile ../../../utils/sexp-json.scm < $input_file \
  | python ../../../c-lisp.py \
  | python ../../../brilisp.py \
  | python ../../../llvm.py > build/$ir_file 

clang -c -o build/$kernel_object build/$ir_file
clang -c -o build/runtime.o kernel_test.c
clang -o build/$executable_file build/runtime.o build/$kernel_object

./build/$executable_file


