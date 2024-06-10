;; Result variable numbering
(define tmp-num 0)
(define (tmp-sym)
    (set! tmp-num (+ tmp-num 1))
    (string->symbol (string-append "tmp_" (number->string tmp-num))))

;; Utility accessors
(define (first expr) (list-ref expr 0))


;; Arithmetic operator symbols and corresponding BRIL opcodes
(define arith-ops
    '((+ . add)
      (- . sub)
      (* . mul)
      (/ . div)))
(define (get-arith-op expr)
    (assq-ref arith-ops expr))

(define (emit-set name type op args)
    `((set (,name ,type) ,(append `(,op) args))))


;; Procedures to emit code
;; Each procedure here returns a list containing complete Brilisp
;; instructions (even if it's just one instruction).
;; Each emitted list takes inputs from `tmp_*` variables and puts
;; its result in the variable `res`
(define (emit-expr expr)
    (cond
        ((symbol? expr) (emit-id-expr expr))
        ((number? expr) (emit-const-int-expr expr))
        ((get-arith-op (first expr)) (emit-arith-expr expr))
        ((symbol? (first expr)) (emit-call-expr expr))))

(define (emit-arith-expr expr)
    (let ((op (get-arith-op (first expr)))
          (arg1 (emit-expr (list-ref expr 1)))
          (arg2 (emit-expr (list-ref expr 2)))
          (err (if (eq? (length expr) 3) 0 (error "Arithmetic operations take exactly 2 arguments:" expr)))
          (in1 (tmp-sym))
          (in2 (tmp-sym)))

        (append!
            arg1
            (emit-set in1 'int 'id `(res))
            arg2
            (emit-set in2 'int 'id `(res))
            (emit-set 'res 'int op `(,in1 ,in2)))))

(define (emit-id-expr expr)
    (emit-set 'res 'int 'id `(,expr)))

(define (emit-const-int-expr expr)
    (emit-set 'res 'int 'const `(,expr)))

(define (emit-call-expr expr)
    (define arg-syms '())
    (define arg-ins '())
    (for-each
        (lambda (a)
            (define arg-sym (tmp-sym))
            (set! arg-syms (append arg-syms `(,arg-sym)))
            (set! arg-ins (append
                arg-ins
                (emit-expr a)
                (emit-set arg-sym 'int 'id '(res)))))
        (list-tail expr 1))
    (append
        arg-ins
        (emit-set 'res 'int 'call (append `(,(first expr)) arg-syms))))

(display
    (emit-expr
        '(print (/ (* 4 6) (add3 1 2 3)))))
