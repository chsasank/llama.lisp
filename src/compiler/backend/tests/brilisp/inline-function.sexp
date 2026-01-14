;; CMD: guile ../../utils/sexp-json.scm < {filename} | python ../../brilisp.py | python ../../llvm.py

(brilisp
    (define ((llvm.memcpy.p0i8.p0i8.i64 void) (dest (ptr int8)) (src (ptr int8)) (len int) (isvolatile bool)))
    (define-inline-brilisp ((memcpy void) (dst (ptr int8)) (src (ptr int8)) (n int))
        (set (tmp void) (call llvm.memcpy.p0i8.p0i8.i64 dst src n #f))
        (ret)))
