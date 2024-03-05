(load "rewrite.lisp")

(fl-let 'IP '(comp (insert add) (alpha mul) trans))
(fl-let 'MM '(comp (alpha (alpha IP) (alpha distl) distr
                 (cat (idx 0) (comp trans (idx 1))))))
(fl-let 'C-IP
    '(for-loop i1 (const 0) (const 2)
        (for-loop i2 (const 0) (comp len (idx 0))
                    (comp (idx i2) (idx i1)))))

(defvar *test-cases* `(
  ; function, expected value
  ((normalize-comp '(alpha g))
   (alpha g))

  ((normalize-comp '(comp (comp f g) h))
   (comp f g h))

  ((normalize-comp '(comp f1 f2 (comp f3 f4) f5))
   (comp f1 f2 f3 f4 f5))

  ((normalize-comp '(alpha (comp f1 f2 (comp f3 f4) f5)))
   (alpha (comp f1 f2 f3 f4 f5)))

  ((normalize-comp '(comp f1 (comp f2 (comp f3 f4) f5)))
   (comp f1 f2 f3 f4 f5))

  ((normalize-comp '(comp g (alpha
                      (comp f1 (comp f2 (comp f3 f4) f5)))))
   (comp g (alpha (comp f1 f2 f3 f4 f5))))

  ((normalize-comp (fl-expand '(comp IP C-IP)))
   (comp (insert add) (alpha mul) trans
    (for-loop i1 (const 0) (const 2)
      (for-loop i2 (const 0) (comp len (idx 0))
        (comp (idx i2) (idx i1))))))

  ((apply-comp-based-rule "2-53"
    '(comp (insert add) (alpha mul)
        (for-loop i1 (const 0)
          len (idx i1))))
   (comp (insert add)
     (for-loop i1 (const 0) len
      (comp mul (idx i1)))))

  ; without recursion, this rule should fail
  ((apply-rule "2-53"
    '(comp (insert add) (comp (alpha f) (for-loop i g h E))))
   (comp (insert add) (comp (alpha f) (for-loop i g h E))))

  ; rule passes with recursion
  ((apply-rule-recursively "2-53"
    '(comp (insert add) (comp (alpha f) (for-loop i g h E))))
   (comp (insert add) (comp (for-loop i g h (comp f E)))))

  ; new rule for normalization
  ((normalize-comp '(comp (insert add) (comp
                     (for-loop i g h (comp f E)))))
   (comp (insert add) (for-loop i g h (comp f E))))

  ; this rule doesn't work without 2-49
  ((apply-rule-recursively "2-53"
    (normalize-comp (fl-expand '(comp IP C-IP))))
   ,(normalize-comp (fl-expand '(comp IP C-IP))))

  ; see [2], eq 49
  ((apply-rule-recursively "2-49"
    (normalize-comp (fl-expand '(comp IP C-IP))))
   (comp (insert add) (alpha mul)
    (for-loop i2 (const 0) (comp len (idx 0))
      (for-loop i1 (const 0) (const 2)
        (comp (idx i2) (idx i1))))))
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

(test-driver)