(load "pat-match.lisp")
; TODO: move to asdy system
; ref: https://lispcookbook.github.io/cl-cookbook/systems.html

;;; Style of rule ref [1: 39] means ref 1 equation 39
;;; References
;;; 1. Backus Turing Lecture
;;; 2. FP
(defparameter *fl-rules* '(
  ; (lhs rhs rule-reference)
  ; TODO: make object
  ((comp trans
    (for-loop ?j ?r ?s
      (for-loop ?i ?f ?g ?E)))
   (for-loop ?i ?f ?g
     (for-loop ?j ?r ?s ?E))
    "[2:49]")
  ((comp (alpha ?f) (for-loop ?i ?g ?h ?E))
   (for-loop ?i ?g ?h (comp ?f ?E))
   "[2: 53]")
))

(defun lhs (rule)
  (first rule))

(defun rhs (rule)
  (second rule))

(defun program-rewrite (prog rule)
  ; need recursive search
  (let ((bindings (pat-match (lhs rule) prog)))
    (if bindings
      (sublis bindings (rhs rule))
      prog)))

(let ((prog '(comp trans (for-loop j (const 0) len
    (for-loop i (const 0) (comp len (idx 0))
        (comp (idx i) (idx j)))))))
  (setf prog (program-rewrite prog (first *fl-rules*)))
  (setf prog (program-rewrite prog (second *fl-rules*)))
  (print prog))

