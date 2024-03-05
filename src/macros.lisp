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
          ((integerp var-shape) var-shape)
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

(defun add-fl-shortcut (fn-name body)
  "Add fl function to *fl-env"
  (if (assoc fn-name  *fl-env*)
    (setf (second (assoc fn-name *fl-env*)) body)
    (push (cons fn-name body) *fl-env*)))

(defun get-fl-shortcut (fn-name)
  (if (assoc fn-name *fl-env*)
    (cdr (assoc fn-name *fl-env*))
    nil))

(defun expand-fl-shortcuts (fl-x)
  "Recursively expand all the shortcuts used in the body"
  (cond
    ((symbolp fl-x) 
      (let ((shortcut (get-fl-shortcut fl-x)))
        (if shortcut (expand-fl-shortcuts shortcut) fl-x)))
    ((listp fl-x) (mapcar #'expand-fl-shortcuts fl-x))
    (t fl-x)))

(defmacro fl-let (name body)
  `(add-fl-shortcut ',name ',body))

;; example usages
; (pprint (structure-operator '(3 ? 5)))
; (add-fl-shortcut 'IP '(comp (insert add) (alpha mul) trans))
; (fl-let MM (comp (alpha (alpha IP) (alpha distl) distr 
;                  (cat (idx 0) (comp trans (idx 1))))))
; (print (get-fl-shortcut 'MM))
; (print (expand-fl-shortcuts 'IP))
; (print (expand-fl-shortcuts 'MM))
