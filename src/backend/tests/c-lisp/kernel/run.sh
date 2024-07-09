guile ../../../utils/sexp-json.scm < $1 | python ../../../c-lisp.py | python ../../../brilisp.py | python ../../../llvm.py > ir.ll
clang ir.ll test_kernel.c -Wno-override-module -Wno-implicit-const-int-float-conversion -o kernel

./kernel

rm ./kernel ./ir.ll
