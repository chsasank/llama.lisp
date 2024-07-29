DATALAYOUT = target datalayout = \"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v16:16:16-v32:32:32-v64:64:64-v128:128:128-n16:32:64\"
TRIPLE = target triple = \"nvptx64-nvidia-cuda\"

# C-Lisp kernel to LLVM
%.ll: %.sexp
	guile ../../../utils/sexp-json.scm < $< | python ../../../c-lisp.py | python ../../../brilisp.py | python ../../../llvm.py > $@
	sed -e "s/^target datalayout.*/$(DATALAYOUT)/" -i $@
	sed -e "s/^target triple.*/$(TRIPLE)/" -i $@
	echo '!nvvm.annotations = !{!0}' >> $@
	echo '!0 = !{void (float addrspace(1)*, float addrspace(1)*, float addrspace(1)*)* @kernel, !"kernel", i32 1}' >> $@

# C-Lisp driver to LLVM
%.driver.ll: %.driver.sexp %_macros.py
	guile ../../../utils/sexp-json.scm < $(word 1, $^) | python ../../../prelisp.py cuda_macros.py | python ../../../c-lisp.py | python ../../../brilisp.py | python ../../../llvm.py | sed -e '/^target/d' > $@

# LLVM kernel to PTX
%.ptx: %.ll
	llc -mcpu=sm_75 -O0 -o $@ $<

# Driver/host-side control code
%.driver: %.driver.ll
	clang -o $@ -lcuda -lm $<

%.run: %.driver %.ptx
	LD_LIBRARY_PATH="/lib/x86_64-linux-gnu/" ./$(word 1, $^) < $(word 2, $^)
