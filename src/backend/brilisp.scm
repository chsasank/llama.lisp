(use-modules (json))

; utility functions
(define (first x)
    (car x))

(define (second x)
    (cadr x))

(define (third x)
    (caddr x))

(define (rest x)
    (cdr x))

(define (map-vec fn l)
    (list->vector (map fn l)))

; entry
(define (bril expr)
    (scm->json `((functions . ,(map-vec (lambda (x)
        (if (function? x)
            (emit-function x)
            (error "syntax error: " x)))
        expr)))))

(define (function? expr)
    (and (pair? expr) (eq? (car expr) 'bril-define)))

(define (emit-function expr)
    (let* ((header (second expr))
           (name (first (first header)))
           (type (second (first header)))
           (args (rest header))
           (instrs (rest (rest expr))))
        `((name . ,name)
          (type . ,(gen-type type))
          (args . ,(map-vec gen-arg args))
          (instrs . ,(map-vec gen-instr instrs)))))

(define (gen-arg arg)
    `((name . ,(first arg)) (type . ,(gen-type (second arg)))))

(define (gen-type type)
    (if (list? type)
        `((,(first type) . ,(gen-type (second type))))
        type))

(define (gen-instr instr)
    (define (const? instr)
        (and (eq? (first instr) 'set)
             (eq? (first (third instr)) 'const)))
    
    (define (gen-const-instr instr)
        (let ((to (second instr))
              (from (third instr)))
            `((op . const)
              (type . ,(gen-type (second to)))
              (dest . ,(first to))
              (value . ,(second from)))))

    (define (value? instr)
        (and (eq? (first instr) 'set)
             (memq (first (third instr))
                '(add mul sub div eq lt gt le ge not and or alloc load ptradd id fadd fsub fmul fdiv))))

    (define (gen-value-instr instr)
        (let ((to (second instr))
              (from (third instr)))
            `((op . ,(first from))
              (type . ,(gen-type (second to)))
              (dest . ,(first to))
              (args . ,(list->vector (rest from))))))

    (define (ret? instr)
        (eq? (first instr) 'ret))
    
    (define (gen-ret-instr instr)
        `((op . ret) (args . ,(list->vector (rest instr)))))

    (define (call? instr)
        (and (eq? (first instr) 'set)
             (eq? (first (third instr)) 'call)))
    
    (define (gen-call-instr instr)
        (let ((to (second instr))
              (from (third instr)))
            `((op . call)
              (type . ,(gen-type (second to)))
              (dest . ,(first to))
              (funcs . ,(vector (second from)))
              (args . ,(list->vector (rest (rest from)))))))
    
    (define (jmp? instr)
        (eq? (first instr) 'jmp))
    
    (define (gen-jmp-instr instr)
        `((op . jmp)
          (labels . ,(list->vector (rest instr)))))

    (define (label? instr)
        (eq? (first instr) 'label))
    
    (define (gen-label-instr instr)
        `((label . ,(second instr))))
    
    (define (br? instr)
        (eq? (first instr) 'br))
    
    (define (gen-br-instr instr)
        `((op . br)
          (args . ,(vector (second instr)))
          (labels . ,(list->vector (rest (rest instr))))))

    (define (nop? instr)
        (eq? (first instr) 'nop))

    (define (gen-nop-instr instr)
        `((op . nop)))

    (define (store? instr)
        (eq? (first instr) 'store))

    (define (gen-store-instr instr)
        `((op . store)
          (args . ,(list->vector (rest instr)))))

    (cond
        ((const? instr) (gen-const-instr instr))
        ((value? instr) (gen-value-instr instr))
        ((ret? instr) (gen-ret-instr instr))
        ((call? instr) (gen-call-instr instr))
        ((jmp? instr) (gen-jmp-instr instr))
        ((label? instr) (gen-label-instr instr))
        ((br? instr) (gen-br-instr instr))
        ((nop? instr) (gen-nop-instr instr))
        ((store? instr) (gen-store-instr instr))
        (else (error "unknown instruction: " instr))))

(bril (rest (read)))
