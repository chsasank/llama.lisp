;;; Syntactic sugar/macros to transform to the base language
(defun structure-operator (shapes)
  (let ((dim (length shapes))
        (all-vars (mapcar #'(lambda (x) (gensym "i")) shapes)))
    (defun generate-body (vars)
      (if (eq (length vars) 1)
        `(idx ,(first vars))
        `(comp ,@(mapcar
                  #'(lambda (x) (list 'idx x)) (reverse vars)))))

    (defun generate-len (vars)
      (let* ((var-index (- dim (length vars)))
             (var-shape (nth var-index shapes)))
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

(pprint (structure-operator '(3 ? 5)))