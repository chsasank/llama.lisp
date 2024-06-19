;; Tasks:
;; * Register macros
;; * Parse body

(define (eval-program pgm macros)
    (cond
        ((not (pair? pgm)) pgm)
        ;((eq? (car pgm) 'eval-macro) (apply (assq-ref (car (cdr pgm)) macros) (cddr pgm)))
        ((eq? (car pgm) 'eval-macro) (apply (assq-ref macros (car (cdr pgm))) (cddr pgm)))
        (else
            (map-in-order
                (lambda (x)
                    (eval-program x macros))
                pgm))))

(define (eval-macro-def macro-def)
    (let
        ((name (car macro-def))
         (body (eval (car (cdr macro-def)) (interaction-environment))))
        (if (procedure? body)
            `(,name . ,body)
            (error "Not a procedure:" body))))

(define (prelisp expr)
    (let*
        ((macros '(()))
         (macros (map (lambda (m) (eval-macro-def m)) (car (cdr expr))))
         (pgm (car (cddr expr))))
        ;(display macros)
        (eval-program pgm macros)))

(display (prelisp (read)))
