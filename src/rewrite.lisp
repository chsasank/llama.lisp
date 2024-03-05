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
(defun normalize-comp-step (prog)
  "Heuristic to make further analysis easy.
  Ideally should be derivable from above."
  (if (not (listp prog))
    prog
    (let ((binding-simple (pat-match '(comp ?inside-comp) prog)))
      (if binding-simple
        (cdr (assoc 'inside-comp binding-simple))
        ; try yet another pattern
        (let* ((comp-pat '(comp (?* ?start-comp)
                              (comp (?* ?inside-comp))
                              (?* ?end-comp)))
               (binding (pat-match comp-pat prog)))
          (if binding
            `(comp ,@(cdr (assoc '?start-comp binding))
                  ,@(cdr (assoc '?inside-comp binding))
                  ,@(cdr (assoc '?end-comp binding)))
            
            ; try recursively if this too failed
            (mapcar #'normalize-comp-step prog)))))))

(defun normalize-comp (prog)
  "Iteratively apply normalize step until prog converges"
  (let ((old-prog prog)
        (new-prog (normalize-comp-step prog)))
    (loop while (not (equal new-prog old-prog))
      do (setf old-prog new-prog)
      do (setf new-prog (normalize-comp-step old-prog)))
    new-prog))

(defun check-if-comp-based-rule (rule)
  "Check if rule is comp based"
  (eq (first (first rule)) 'comp))

(defun apply-comp-based-rule (rule prog)
  "Apply composition based rules.
   Heursitic to speed up search.
   Normalize prog for best performance"
  (if (not (check-if-comp-based-rule rule))
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

(defun apply-rule (rule prog)
  (if (check-if-comp-based-rule rule)
    (apply-comp-based-rule rule prog)
    (let ((bindings (pat-match (first rewrite-rule) prog)))
      (if bindings
        (sublis bindings (second rewrite-rule)))
        prog)))

(defun apply-rule-recursively (rule prog)
  (if (not (listp prog))
    prog
    (let ((new-prog (apply-rule rule prog)))
      (if (equal new-prog prog)
        ; don't give up yet
        (mapcar #'(lambda (subprog)
                    (apply-rule-recursively rule subprog))
          prog)
        new-prog))))
