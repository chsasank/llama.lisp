;; CMD: guile ../../utils/sexp-json.scm < {filename} | python ../../cuda-lisp.py | guile ../../utils/json-sexp.scm

;python ../../c-lisp.py | python ../../brilisp.py | python ../../llvm.py
(cuda-lisp
    ;  __shared__ int s[64];
    (define-shared s [64 int])
    (define-kernel ((staticReverse void) (d (ptr int)) (n int))
        ; int t = threadIdx.x;
        (declare t int)
        (set t (tid.x))

        ; int tr = n-t-1;
        (declare tr int)
        (set tr (sub (sub n t) 1))

        ; s[t] = d[t];
        (declare s_t (ptr int))
        (set s_t (aptradd shared t))
        (declare d_t (ptr int))
        (set d_t (ptradd d t))
        (store s_t (load d_t))

        (__syncthreads)

        ; d[t] = s[tr];
        (declare s_tr (ptr int))
        (set s_tr (aptradd shared tr))
        (store d_t (load s_tr))

        (ret))


)
