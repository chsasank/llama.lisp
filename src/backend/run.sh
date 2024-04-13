tmp_in=$(mktemp --suffix '.ll')
tmp_out=$(mktemp --suffix '.out')

cp /dev/stdin $tmp_in
clang $tmp_in runtime.c -o $tmp_out -Wno-override-module
$tmp_out

rm $tmp_in $tmp_out