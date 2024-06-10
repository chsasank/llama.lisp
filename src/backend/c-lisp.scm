;; Result variable numbering
(define (tmp-sym)
    (string->symbol (string-append
        "tmp_c-lisp_"
        (number->string (+ (random 899999) 100000))))) ; a 5-digit number
(define res-sym (make-object-property)) ; Holds the result of a block of instructions


;; Utilities
(define (first expr) (list-ref expr 0))

; Combine the instruction lists in ins-list, and
; set (res-sym) of the result
(define (create-ins-list res . ins-list)
    (let ((ins (apply append ins-list)))
        (set! (res-sym ins) res)
        ins))


;; Arithmetic operator symbols and corresponding BRIL opcodes
(define arith-ops
    '((+ . add)
      (- . sub)
      (* . mul)
      (/ . div)))

;; Statement classification
(define (get-arith-op expr)
    (assq-ref arith-ops expr))

(define (call? expr)
    (eq? (first expr) 'call))

(define (lvalue? expr)
    (symbol? expr))

;; Procedures to emit code
;; Each procedure here returns a list containing complete Brilisp
;; instructions (even if it's just one instruction).
;; The result of a list of Brilisp expressions is stored
;; in object property res-sym.
(define (emit-set name type op args)
    (create-ins-list
        name
        `((set (,name ,type) ,(append `(,op) args)))))

(define (emit-expr expr)
    (cond
        ((symbol? expr) (emit-id-expr expr))
        ((number? expr) (emit-const-int-expr expr))
        ((get-arith-op (first expr)) (emit-arith-expr expr))
        ((call? expr) (emit-call-expr expr))
        ((eq? (first expr) 'set) (emit-set-expr expr))
        (else (error "Bad syntax:" expr))))

(define (emit-arith-expr expr)
    (let ((op (get-arith-op (first expr)))
          (arg1 (emit-expr (list-ref expr 1)))
          (arg2 (emit-expr (list-ref expr 2)))
          (err (if (eq? (length expr) 3) 0 (error "Arithmetic operations take exactly 2 arguments:" expr)))
          (res (tmp-sym)))
        (create-ins-list
            res
            arg1
            arg2
            (emit-set res 'int op `(,(res-sym arg1) ,(res-sym arg2))))))

(define (emit-id-expr expr)
    (emit-set (tmp-sym) 'int 'id `(,expr)))

(define (emit-const-int-expr expr)
    (emit-set (tmp-sym) 'int 'const `(,expr)))

(define (emit-set-expr expr)
    (cond
        ((not (eq? (length expr) 3)) (error "Set operation takes one lvalue and 1 other argument:" expr))
        ((not (lvalue? (list-ref expr 1))) (error "Set operation destination must be an lvalue:"))
        (else
            (let* ((val-ins (emit-expr (list-ref expr 2)))
                   (val-sym (res-sym val-ins))
                   (name (list-ref expr 1)))
                (create-ins-list
                    name
                        val-ins
                        (emit-set name 'int 'id `(,val-sym)))))))

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
        (list-tail expr 2))
    (create-ins-list
        res
        arg-ins-list
        (emit-set res 'int 'call (append `(,(list-ref expr 1)) arg-syms))))


(display
    (emit-expr
        '(set res
            (/
                (set a (* 4 6))
                (call add3 1 2 3)))))
