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
          (type . ,type)
          (args . ,(map-vec gen-arg args))
          (instrs . ,(map-vec gen-instr instrs)))))

(define (gen-arg arg)
    `((name . ,(first arg)) (type . ,(second arg))))

(define (gen-instr instr)
    (define (const? instr)
        (and (eq? (first instr) 'set)
             (eq? (first (third instr)) 'const)))
    
    (define (gen-const-instr instr)
        (let ((to (second instr))
              (from (third instr)))
            `((op . const)
              (type . ,(second to))
              (dest . ,(first to))
              (value . ,(second from)))))

    (define (value? instr)
        (and (eq? (first instr) 'set)
             (memq (first (third instr))
                '(add mul sub div eq lt gt le ge not and or))))

    (define (gen-value-instr instr)
        (let ((to (second instr))
              (from (third instr)))
            `((op . ,(first from))
              (type . ,(second to))
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
              (type . ,(second to))
              (dest . ,(first to))
              (funcs . ,(vector (second from)))
              (args . ,(list->vector (rest (rest from)))))))

    (define (print? instr)
        (eq? (first instr) 'print))
    
    (define (gen-print-instr instr)
        `((op . print) (args . ,(list->vector (rest instr)))))

    (cond
        ((const? instr) (gen-const-instr instr))
        ((value? instr) (gen-value-instr instr))
        ((ret? instr) (gen-ret-instr instr))
        ((call? instr) (gen-call-instr instr))
        ((print? instr) (gen-print-instr instr))
        (else (error "unknown instruction: " instr))))

(bril '(
    (bril-define ((add5 int) (n int))
        (set (five int) (const 5))
        (set (sum int) (add n five))
        (ret sum))

    (bril-define ((main int))
        (set (a int) (const 9))
        (set (b int) (call add5 a))
        (ret b)
    )
))
