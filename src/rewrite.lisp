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

;; Normalize comps for faster convergence
(defun normalize-comps-step (prog)
  "Heuristic to make further analysis easy.
  Ideally should be derivable from above."
  (if (not (listp prog))
    prog
    (let* ((comp-pat '(comp (?* ?start-comp)
                          (comp (?* ?inside-comp))
                          (?* ?end-comp)))
           (binding (pat-match comp-pat prog)))
    (if binding
      `(comp ,@(cdr (assoc '?start-comp binding))
             ,@(cdr (assoc '?inside-comp binding))
             ,@(cdr (assoc '?end-comp binding)))
        ; try recursively if failed
        (mapcar #'normalize-comps prog)))))

(defun normalize-comps (prog)
  "Iteratively apply normalize step until prog converges"
  (let ((old-prog prog)
        (new-prog (normalize-comps-step prog)))
    (loop while (not (equal new-prog old-prog))
      do (setf old-prog new-prog)
      do (setf new-prog (normalize-comps-step old-prog)))
    new-prog))

(defun apply-comp-based-rule (comp-rule prog)
  "Apply composition based rules.
  Assumes prog is normalized
  "
  (if (not (eq (first (lhs rule) 'comp)))
    (error "rule ~a doesn't start with comp" comp-rule))

  (let* ((comp-pat `(comp (?* ?start-comp)
                          ,@(rest (lhs rule))
                          (?* ?end-comp)))
         (binding (pat-match comp-pat prog)))
    (if binding
      `(comp ,@(cdr (assoc '?start-comp binding))
             ,@(sublis binding (rhs rule))
             ,@(cdr (assoc '?end-comp binding)))
        prog)))

; (defun generate-rewrite-rules (rule)
;   "Sort of like meta rules of each rule"
;   ; lhs = rhs => rhs = lhs
;   (list
;     (list (lhs rule) (rhs rule))
;     (list (rhs rule) (lhs rule))))

(defun rewrite-program (prog rewrite-rule)
  (let ((bindings (pat-match (first rewrite-rule) prog)))
    (if bindings
      (sublis bindings (second rewrite-rule)))))
