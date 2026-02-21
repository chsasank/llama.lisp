(use-modules (ice-9 pretty-print))

(define (pprint expr) (pretty-print expr))
(define (first expr) (car expr))
(define (rest expr) (cdr expr))
(define (idx expr n)
    (list-ref expr n))
(define (second expr) (idx expr 1))

(define tmp-symbol-counter 0)
(define (tmp-symbol)
    (set! tmp-symbol-counter (+ tmp-symbol-counter 1))
    (string->symbol (string-append "prelisp-" (number->string tmp-symbol-counter))))


(define (prelisp expr)
    (if (not (eq? (first expr) 'prelisp))
        (error "not prelisp"))
    
    (if (not (eq? (length expr) 3))
        (error "not prelisp"))
    

    (let ((env (first (rest expr)))
          (body (second (rest expr))))

        ; evaluate env
        (cond 
            ((eq? (first env) 'guile) (map primitive-eval (rest env)))
            (else (error "unknown macro env")))

        (primitive-eval (list 'quasiquote body))))


(pretty-print (prelisp (read)))
