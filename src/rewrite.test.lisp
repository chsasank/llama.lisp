(load "rewrite.lisp")

(fl-let 'IP '(comp (insert add) (alpha mul) trans))
(fl-let 'MM '(comp (alpha (alpha IP) (alpha distl) distr 
                 (cat (idx 0) (comp trans (idx 1))))))
(fl-let 'C-IP
    '(for-loop i1 (const 0) (const 2)
        (for-loop i2 (const 0) (comp len (idx 0))
                    (comp (idx i2) (idx i1)))))

(defvar *test-cases* '(
  ; function, expected value
  ((normalize-comps '(alpha g))
   (alpha g))

  ((normalize-comps '(comp (comp f g) h))
   (comp f g h))

  ((normalize-comps '(comp f1 f2 (comp f3 f4) f5))
   (comp f1 f2 f3 f4 f5))

  ((normalize-comps '(alpha (comp f1 f2 (comp f3 f4) f5)))
   (alpha (comp f1 f2 f3 f4 f5)))

   ((normalize-comps '(comp f1 (comp f2 (comp f3 f4) f5)))
    (comp f1 f2 f3 f4 f5))

   ((normalize-comps '(comp g (alpha (comp f1 (comp f2 (comp f3 f4) f5)))))
    (comp g (alpha (comp f1 f2 f3 f4 f5))))

    ((normalize-comps (fl-expand '(comp IP C-IP)))
     (comp (insert add) (alpha mul) trans
        (for-loop i1 (const 0) (const 2)
            (for-loop i2 (const 0) (comp len (idx 0))
                (comp (idx i2) (idx i1))))))

;    ((apply-comp-based-rule '2-53))
  
))

(defun test-driver ()
  "Run all test cases"
  (mapcar
    #'(lambda (test-case)
      (let* ((expression (first test-case))
             (expected (second test-case))
             (actual (eval expression)))
        (if (equal expected actual)
          (format T "~a passed~%" expression)
          (error "~a failed: expected ~a but got ~a~%"
            expression expected actual))))
    *test-cases*))


; (let ((prog (fl-expand '(comp IP HY))))
;   (print prog)
;   (print "Original program")
;   (print (normalize-comps prog))
;   ; (print 
;   ;   (mapcar
;   ;     #'(lambda (x) (rewrite-program prog x))
;   ;     (generate-rewrite-rules '1-def-comp)))
      
; )
(test-driver)