;; CMD: guile ../../utils/sexp-json.scm < {filename} | python ../../brilisp.py | python ../../llvm.py

(brilisp
    (define-struct int4 (int int int int))
    (define ((vector-load (struct int4)) (src (ptr float)) (pred bool))
        (set (vec (struct int4)) 
            (asm "mov.u32 $0, 0x0; mov.u32 $1, 0x0; mov.u32 $2, 0x0; mov.u32 $3, 0x0; @$5 ld.global.v4.b32 { $0, $1, $2, $3 }, [ $4 + 0 ];"
                 "=r,=r,=r,=r,l,b"
                 src pred))
        (ret vec)))
