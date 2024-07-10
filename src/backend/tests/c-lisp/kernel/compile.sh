#!/bin/bash

if [ -z "$1" ]; then
    echo "No input argument provided."
    exit 1
fi

base_name="$1"
build_dir="build"

if [ ! -d "$build_dir" ]; then
    mkdir "$build_dir"
fi

input_file="$base_name.sexp"
kernel_object="$base_name.o"
ir_file="$base_name.ll"
executable_file="$base_name.out"

guile ../../../utils/sexp-json.scm < "$input_file" \
  | python ../../../c-lisp.py \
  | python ../../../brilisp.py \
  | python ../../../llvm.py > "$build_dir/$ir_file"

clang -c -o "$build_dir/$kernel_object" "$build_dir/$ir_file"
clang -c -o "$build_dir/runtime.o" kernel_test.c
clang -o "$build_dir/$executable_file" "$build_dir/runtime.o" "$build_dir/$kernel_object"

# Run executable
"./$build_dir/$executable_file"
