;;; Syntactic sugar/macros to transform to the base language
(defun structure-operator (shape)
  "This will be basis for type system and optimization"
  (let ((dim (length shape))
        (all-vars (mapcar #'(lambda (x) (gensym "i")) shape)))
    (defun generate-body (vars)
      (if (eq (length vars) 1)
        `(idx ,(first vars))
        `(comp ,@(mapcar
                  #'(lambda (x) (list 'idx x)) (reverse vars)))))

    (defun generate-len (vars)
      (let* ((var-index (- dim (length vars)))
             (var-shape (nth var-index shape)))
        (cond
          ((integerp var-shape) `(const ,var-shape))
          ((eq var-index 0) 'len)
          (t `(comp len ,@(make-list var-index
                        :initial-element '(idx 0)))))))
      
    (defun generate-loops (vars)
      (if (eq (length vars) 0)
        (generate-body all-vars)
        `(for-loop
            ,(first vars)
            (const 0) 
            ,(generate-len vars)
            ,(generate-loops (rest vars)))))

      (generate-loops all-vars)))

(defvar *fl-env* nil
  "A list of all functions avialable for rewrite.")

(defun fl-let (fn-name body)
  "Add fl function to *fl-env"
  (if (assoc fn-name  *fl-env*)
    (setf (second (assoc fn-name *fl-env*)) body)
    (push (cons fn-name body) *fl-env*)))

(defun fl-expand (fl-x)
  "Recursively expand all the shortcuts used in the body"
  (cond
    ((symbolp fl-x) 
      (let ((shortcut (assoc fl-x *fl-env*)))
        (if shortcut
            (fl-expand (cdr shortcut))
            fl-x)))
    ((listp fl-x) (mapcar #'fl-expand fl-x))
    (t fl-x)))

;; example usages
; (pprint (structure-operator '(3 ? 5)))
; (fl-let 'IP '(comp (insert add) (alpha mul) trans))
; (fl-let 'MM '(comp (alpha (alpha IP) (alpha distl) distr 
;                    (cat (idx 0) (comp trans (idx 1))))))
; (print (fl-expand 'MM))
