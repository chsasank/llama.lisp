(load "pat-match.lisp")

(defparameter *fl-rules* '(
  ; (lhs rhs rule-reference)
  (; lhs
    (comp trans
      (for-loop ?j ?r ?s
        (for-loop ?i ?f ?g ?E)))
    ; rhs
    (for-loop ?i ?f ?g
      (for-loop ?j ?r ?s ?E))
    ; rule name
    "[2:49]")
  

))


(defun program-rewrite (prog rule)
  (let ((lhs (first (first *fl-rules*)))
        (rhs (second (first *fl-rules*)))
        (prog '(comp trans (for-loop j (const 0) len
              (for-loop i (const 0) (comp len (idx 0))
              (comp (idx i) (idx j)))))))
        (sublis (pat-match lhs prog) rhs)))

; Style of rule ref [1: 39] means ref 1 equation 39
; References
; 1. Backus Turing Lecture
; 2. FP