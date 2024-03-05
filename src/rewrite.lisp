;;; Style of rule ref [1: 39] means ref 1 equation 39
;;; References
;;; 1. Backus Turing Lecture
;;; 2. FP optimization

(load "pat-match.lisp")
(load "macros.lisp")

;; Helper functions for FL rules
(defparameter *fl-equations* nil
  "A list of all rules available for rewrite")

(defun reverse-ref (ref)
  (concatenate 'string ref "-rev"))

(defun add-fl-equation (&key lhs rhs ref)
  "Add reference and its reverse to the rulebase"
  (pushnew (cons (intern ref) (list lhs rhs)) *fl-equations*)
  (pushnew (cons (intern (reverse-ref ref)) (list rhs lhs))
            *fl-equations*))

(defun fl-equation (ref)
  (assoc (intern ref) *fl-equations*))

(defun fl-rewrite (ref)
  (cdr (fl-equation ref)))

;; Add rules
(add-fl-equation
  :ref "2-49"
  :lhs '(comp trans
          (for-loop ?j ?r ?s
            (for-loop ?i ?f ?g ?E)))
  :rhs '(for-loop ?i ?f ?g
          (for-loop ?j ?r ?s ?E)))

(add-fl-equation
  :ref "2-53"
  :lhs '(comp (alpha ?f) (for-loop ?i ?g ?h ?E))
  :rhs '(for-loop ?i ?g ?h (comp ?f ?E)))

;; Normalize comps for faster converge`nce
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

(defun apply-comp-based-rule (rule prog)
  "Apply composition based rules.
   Heursitic to speed up search.
   Normalize prog for best performance"
  (if (not (eq (first (first rule)) 'comp))
    (error "rule ~a doesn't start with comp" rule))

  (let* ((comp-pat `(comp (?* ?start-comp)
                          ,@(rest (first rule))
                          (?* ?end-comp)))
         (binding (pat-match comp-pat prog)))
    (if binding
      `(comp ,@(cdr (assoc '?start-comp binding))
             ,(sublis binding (second rule))
             ,@(cdr (assoc '?end-comp binding)))
        prog)))

(defun rewrite-program (prog rule)
  (let ((bindings (pat-match (first rewrite-rule) prog)))
    (if bindings
      (sublis bindings (second rewrite-rule)))))
