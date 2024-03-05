;;; Style of rule ref [1: 39] means ref 1 equation 39
;;; References
;;; 1. Backus Turing Lecture
;;; 2. FP optimization

(load "pat-match.lisp")
(load "macros.lisp")

;; Helper functions for FL rules
(defparameter *fl-rules* nil
  "A list of all rules available for rewrite")

(defun add-fl-rule (&key lhs rhs ref)
  (pushnew (cons ref (list lhs rhs)) *fl-rules*))

(defun fl-rule (ref)
  (assoc ref *fl-rules*))

(defun lhs (rule)
  (second (assoc rule *fl-rules*)))

(defun rhs (rule)
  (third (assoc rule *fl-rules*)))

;; Add rules
(add-fl-rule
  :ref '2-49
  :lhs '(comp trans
          (for-loop ?j ?r ?s
            (for-loop ?i ?f ?g ?E)))
  :rhs '(for-loop ?i ?f ?g
          (for-loop ?j ?r ?s ?E)))

(add-fl-rule
  :ref '2-53
  :lhs '(comp (alpha ?f) (for-loop ?i ?g ?h ?E))
  :rhs '(for-loop ?i ?g ?h (comp ?f ?E)))

(defun program-rewrite (prog rule)
  ; need recursive search
  (let ((bindings (pat-match (lhs rule) prog)))
    (if bindings
      (sublis bindings (rhs rule))
      prog)))

;;; testing
(fl-let 'IP '(comp (insert add) (alpha mul) trans))
(fl-let 'MM '(comp (alpha (alpha IP) (alpha distl) distr 
                 (cat (idx 0) (comp trans (idx 1))))))
(fl-let 'C-IP (structure-operator '(2 ?)))

(let ((prog (fl-expand '(comp IP C-IP))))

)