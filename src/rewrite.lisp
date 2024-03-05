;;; Style of rule ref [1: 39] means ref 1 equation 39
;;; References
;;; 1. Backus Turing Lecture
;;; 2. FP optimization

(load "pat-match.lisp")
(load "macros.lisp")

;;; Let's create a different structure
;;; No RHS and stuff
(defparameter *fl-rewrites* nil 
  "A list of all rules available for rewrite")

(defun fl-rewrite (ref)
  "Get rewrite for given reference"
  (second (cdr (assoc (intern ref) *fl-rewrites*))))

(defun fl-pattern (ref)
  "Get pattern for given reference"
  (first (cdr (assoc (intern ref) *fl-rewrites*))))

(defun add-fl-rewrite (&key pattern rewrite ref)
  "Add reference and its reverse to the rulebase"
  (pushnew (cons (intern ref) (list pattern rewrite)) *fl-rewrites*))

(add-fl-rewrite
  :pattern '(comp (const (?is ?i integerp))
                  (cat (?* ?fn-list)))
  :rewrite #'(lambda (binding)
    (nth (lookup '?i binding) (lookup '?fn-list binding)))
  :ref "2:3")

(add-fl-rewrite
  :pattern '(comp ?inside-comp)
  :rewrite #'(lambda (binding)
    (lookup '?inside-comp binding))
  :ref "2-21-0")

(add-fl-rewrite
  :pattern '(comp (?* ?start-comp)
              (comp (?* ?inside-comp))
              (?* ?end-comp))
  :rewrite #'(lambda (binding)
    `(comp ,@(lookup '?start-comp binding)
           ,@(lookup '?inside-comp binding)
           ,@(lookup '?end-comp binding)))
  :ref "2-21")

(add-fl-rewrite
  :pattern '(comp (cat (?* ?fn-list) ?g))
  :rewrite #'(lambda (binding)
    `(cat ,@(mapcar
              #'(lambda (f) (comp f (lookup '?g binding)))
              (lookup '?fn-list binding))))
  :ref "2:25")

(add-fl-rewrite
  :pattern '(comp trans
              (for-loop ?j ?r ?s
                (for-loop ?i ?f ?g ?E)))
  :rewrite #'(lambda (binding)
    (sublis binding '(for-loop ?i ?f ?g
                        (for-loop ?j ?r ?s ?E))))
  :ref "2-49")

(add-fl-rewrite
  :pattern '(comp (alpha ?f) (for-loop ?i ?g ?h ?E))
  :rewrite #'(lambda (binding)
    (sublis binding '(for-loop ?i ?g ?h (comp ?f ?E))))
  :ref "2-53")

;; Normalize comps for faster converge`nce
(defun normalize-comp-step (prog)
  "Heuristic to make further analysis easy.
  Ideally should be derivable from above."
  (if (not (listp prog))
    prog
    (let ((binding-simple (pat-match (fl-pattern "2-21-0") prog)))
      (if binding-simple
        (funcall (fl-rewrite "2-21-0") binding-simple)
        ; try yet another pattern
        (let ((binding (pat-match (fl-pattern "2-21") prog)))
          (if binding
            (funcall (fl-rewrite "2-21") binding)
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
  (eq (first (fl-pattern rule)) 'comp))

(defun apply-comp-based-rule (rule prog)
  "Apply composition based rules.
   Heursitic to speed up search.
   Normalize prog for best performance"
  (if (not (check-if-comp-based-rule rule))
    (error "rule ~a doesn't start with comp" rule))

  (let* ((comp-pat `(comp (?* ?start-comp)
                          ,@(rest (fl-pattern rule))
                          (?* ?end-comp)))
         (binding (pat-match comp-pat prog)))
    (if binding
      `(comp ,@(cdr (assoc '?start-comp binding))
             ,(funcall (fl-rewrite rule) binding)
             ,@(cdr (assoc '?end-comp binding)))
        prog)))

(defun apply-rule (rule prog)
  (if (check-if-comp-based-rule rule)
    (apply-comp-based-rule rule prog)
    (let ((bindings (pat-match (fl-pattern rule) prog)))
      (if bindings (funcall (fl-rewrite rule) bindings) prog))))

(defun apply-rule-recursively (rule prog)
  (if (not (listp prog))
    prog
    (let ((new-prog (apply-rule rule prog)))
      (if (equal new-prog prog)
        (mapcar #'(lambda (subprog)
                    (apply-rule-recursively rule subprog)) prog)
        new-prog))))
