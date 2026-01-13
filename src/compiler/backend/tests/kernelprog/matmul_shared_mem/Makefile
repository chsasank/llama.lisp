DATALAYOUT = \"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v16:16:16-v32:32:32-v64:64:64-v128:128:128-n16:32:64\"
TRIPLE = \"nvptx64-nvidia-cuda\"

KERNEL_FILE = kernel
DRIVER_EXEC = exec 

.PHONY: all run clean

all: $(DRIVER_EXEC) $(KERNEL_FILE).ptx

%.ll: %.sexp
	guile ../../../utils/sexp-json.scm < $< | python ../../../c-lisp.py | python ../../../brilisp.py | python ../../../llvm.py | opt -p 'strip,mem2reg,simplifycfg' -S > $@
	sed -e "s/^target datalayout.*/target datalayout = $(DATALAYOUT)/" -i $@
	sed -e "s/^target triple.*/target triple = $(TRIPLE)/" -i $@
	echo '!nvvm.annotations = !{!0}' >> $@
	echo '!0 = !{void (i32, i32, i32, float, float addrspace(1)*, float addrspace(1)*, float, float addrspace(1)*)* @kernel, !"kernel", i32 1}' >> $@

%.ptx: %.ll
	llc -mcpu=sm_86 -O3 -o $@ $<

$(DRIVER_EXEC): driver.c
	clang -o $@ -lcuda -lm -I/usr/local/cuda/include $<
 

run: $(DRIVER_EXEC) $(KERNEL_FILE).ptx
	LD_LIBRARY_PATH="/lib/x86_64-linux-gnu/" ./$(DRIVER_EXEC) $(KERNEL_FILE).ptx

clean:
	rm -f *.ll *.ptx $(DRIVER_EXEC)
