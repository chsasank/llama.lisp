(load "rewrite.lisp")

; inner product
(fl-let 'IP '(comp (insert add) (alpha mul) trans))

; matrix multiplication
(fl-let 'MM 
  '(comp
    (alpha (alpha IP))
    (alpha distl) distr
    (cat (idx 0) (comp trans (idx 1)))))

; structure operator for inner product
; not generating strucuture operators to facilitate testing
(fl-let 'C-IP
  '(for-loop i1 (const 0) (const 2)
      (for-loop i2 (const 0) (comp len (idx 0))
                  (comp (idx i2) (idx i1)))))
; matrix identity
(fl-let 'MI
  '(for-loop i1 (const 0) len
      (for-loop i2 (const 0) (comp len (idx 0))
          (comp (idx i2) (idx i1)))))

; structure operator for matmul
; this honestly needs to be better defined
; especially because of assumption A:mxn, B:nxk
(fl-let 'C-MM '(cat 
  (for-loop i1 (const 0) (comp len (idx 0))
    (for-loop i2 (const 0) (comp len (idx 0) (idx 0))
      (comp (idx i2) (idx i1) (idx 0))))
  (for-loop i3 (const 0) (comp len (idx 0) (idx 0))
    (for-loop i4 (const 0) (comp len (idx 0) (idx 1))
      (comp (idx i4) (idx i3) (idx 1))))))

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

  ((apply-comp-based-rule '2-53
    '(comp (insert add) (alpha mul)
        (for-loop i1 (const 0)
          len (idx i1))))
   (comp (insert add)
     (for-loop i1 (const 0) len
      (comp mul (idx i1)))))

  ; without recursion, this rule should fail
  ((apply-rule '2-53
    '(comp (insert add) (comp (alpha f) (for-loop i g h E))))
   (comp (insert add) (comp (alpha f) (for-loop i g h E))))

  ; rule passes with recursion
  ((apply-rule-recursively '2-53
    '(comp (insert add) (comp (alpha f) (for-loop i g h E))))
   (comp (insert add) (comp (for-loop i g h (comp f E)))))

  ; new rule for normalization
  ((normalize-comp '(comp (insert add) (comp
                     (for-loop i g h (comp f E)))))
   (comp (insert add) (for-loop i g h (comp f E))))

  ; this rule doesn't work without 2-49
  ((apply-rule-recursively '2-53
    (normalize-comp (fl-expand '(comp IP C-IP))))
   ,(normalize-comp (fl-expand '(comp IP C-IP))))

  ; see [2], eq 49
  ((apply-rule-recursively '2-49
    (normalize-comp (fl-expand '(comp IP C-IP))))
   (comp (insert add) (alpha mul)
    (for-loop i2 (const 0) (comp len (idx 0))
      (for-loop i1 (const 0) (const 2)
        (comp (idx i2) (idx i1))))))

  ((normalize-comp (apply-rule '2-3 '(comp (idx 1) (cat f1 f2 f3))))
   f2)

  ((normalize-comp (apply-rule '2-25 '(comp (cat f1 f2) g)))
   (cat (comp f1 g) (comp f2 g)))

  ; page 86, ref 2
  ((normalize-comp (apply-rule '2-25
      '(comp (cat (idx 0) (comp trans (idx 1)))
             (cat MI1 MI2))))
   (cat (comp (idx 0) (cat MI1 MI2))
        (comp trans (idx 1) (cat MI1 MI2))))

  ; ref 2, page 86
  ((normalize-comp (apply-rule-recursively '2-3
    '(cat (comp (idx 0) (cat MI1 MI2))
          (comp trans (idx 1) (cat MI1 MI2)))))
   (cat MI1 (comp trans MI2)))

  ((normalize-comp (apply-rule '2-58
    '(comp (for-loop i start end body) g)))
   (for-loop i start end (comp body g)))

  ; ref 2, page 87
  ((normalize-comp (apply-rule '2-58 (fl-expand '(comp MI (idx 1)))))
   (for-loop i1 (const 0) len
      (comp (for-loop i2 (const 0) (comp len (idx 0))
              (comp (idx i2) (idx i1)))
            (idx 1))))
  
  ((normalize-comp (apply-rule '2-14 '(comp (const x) f1)))
   (const x))

  ((normalize-comp (apply-rule '2-51 
    '(comp distr (cat (for-loop i (const 0) len (idx i)) h))))
   (for-loop i (const 0) len (cat (idx i) h)))
  
  ; ref 2, page 87
  ((normalize-comp (apply-rule '2-51
    '(comp distr (cat
        (for-loop i1 (const 0) len(comp 
          (for-loop i2 (const 0) (comp len (idx 0))
            (comp (idx i2) (idx i1))) (idx 1)))
        MI2))))
   (for-loop i1 (const 0) len
    (cat
      (comp (for-loop i2 (const 0) (comp len (idx 0))
        (comp (idx i2) (idx i1))) (idx 1)) 
      MI2)))

  ((normalize-comp (apply-rule '2-52
    '(comp distl (cat h (for-loop i (const 0) len (idx i))))))
   (for-loop i (const 0) len (cat h (idx i))))
  
  ; ref 2, page 87; eq 72
  ((normalize-comp (apply-rule '2-52
    '(comp distl (cat
        (comp E (idx 0))
        (for-loop j (const 0) (comp len (idx 0) (idx 1))
          (comp E1 (idx 1)))))))
    (for-loop j (const 0) (comp len (idx 0) (idx 1))
      (cat (comp E (idx 0)) (comp E1 (idx 1)))))

  ((apply-rule '2-def-5.2 '(for-loop i (const 0) (const 4) (idx i)))
   (cat (idx 0) (idx 1) (idx 2) (idx 3)))

  ; ref 2; page 85
  ((apply-rule '2-def-5.2 '(for-loop j (const 0) (const 2)
                              (comp (idx i) (idx j))))
   (cat (comp (idx i) (idx 0)) (comp (idx i) (idx 1))))

  ; ref 2; page 85
  ((apply-rules-pipeline '(2-49 2-53 2-def-5.2) (fl-expand '(comp IP C-IP)))
   (comp (insert add)
      (for-loop i2 (const 0) (comp len (idx 0))
        (comp mul (cat (comp (idx i2) (idx 0))
                       (comp (idx i2) (idx 1)))))))

  ((normalize-comp (apply-rule '2-56
    '(comp trans (cat (for-loop i1 f g (const i1))
                      (for-loop i2 f g (idx i2))))))
   (for-loop i1 f g (cat (const i1) (idx i1))))

  ; ref 2; page 86-87
  ((apply-rules-pipeline '(2-25 2-3 2-49 2-14 2-51 2-53 2-58
                           2-14 2-52 2-53 2-53 2-56 2-53)
    (fl-expand '(comp MM C-MM)))
  (for-loop i1 (const 0) (comp len (idx 0))
    (for-loop i4 (const 0) (comp len (idx 0) (idx 1))
      (comp (insert add)
        (for-loop i2 (const 0) (comp len (idx 0) (idx 0))
          (comp mul
           (cat (comp (idx i2) (idx i1) (idx 0))
                (comp (idx i4) (idx i2) (idx 1)))))))))

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
          (error "~a failed.~%Expected:~%~a~%Got: ~%~a"
            expression expected actual))))
    *test-cases*))

(test-driver)