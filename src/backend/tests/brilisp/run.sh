#!/bin/sh
cd "$(dirname "$0")"

tmp_dir=$(mktemp -d)
tmp_in=${tmp_dir}/tmp.ll
tmp_out=${tmp_dir}/tmp.out

cat > $tmp_in
clang $tmp_in runtime.c -o $tmp_out -Wno-override-module -O2
$tmp_out $@

rm -r $tmp_dir
