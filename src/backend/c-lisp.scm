;; Result variable numbering
(define (tmp-sym)
    (string->symbol (string-append
        "tmp_c-lisp_"
        (number->string (+ (random 899999) 100000))))) ; a 5-digit number
(define res-sym (make-object-property)) ; Holds the result of a block of instructions


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
    (define ins `((set (,name ,type) ,(append `(,op) args))))
    (set! (res-sym ins) name)
    ins)


;; Procedures to emit code
;; Each procedure here returns a list containing complete Brilisp
;; instructions (even if it's just one instruction).
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
          (res (tmp-sym)))

        (define ins (append
            arg1
            arg2
            (emit-set res 'int op `(,(res-sym arg1) ,(res-sym arg2)))))
        (set! (res-sym ins) res)
        ins))

(define (emit-id-expr expr)
    (emit-set (tmp-sym) 'int 'id `(,expr)))

(define (emit-const-int-expr expr)
    (emit-set (tmp-sym) 'int 'const `(,expr)))

(define (emit-call-expr expr)
    (define arg-syms '())
    (define arg-ins-list '())
    (define res (tmp-sym))
    (for-each
        (lambda (a)
            (define arg-sym (tmp-sym))
            (define arg-ins (emit-expr a))
            (set! arg-syms (append arg-syms `(,arg-sym)))
            (set! arg-ins-list (append
                arg-ins-list
                arg-ins
                (emit-set arg-sym 'int 'id `(,(res-sym arg-ins))))))
        (list-tail expr 1))
    (define ins (append
        arg-ins-list
        (emit-set res 'int 'call (append `(,(first expr)) arg-syms))))
    (set! (res-sym ins) res)
    ins)

(display
    (emit-expr
        '(print (/ (* 4 6) (add3 1 2 3)))))
