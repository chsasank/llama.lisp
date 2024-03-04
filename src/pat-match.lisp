; Lifted from PAIP Chapter 5
(defconstant fail nil "Indicates pat-match failure")

(defconstant no-bindings '((t . t)) 
  "Indicates pat-match success but with no variables")

; functions for assoc
(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun lookup (var bindings)
  "Look up value for var"
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add (var . val) to binding list"
  (cons (cons var val) 
    (if (eq bindings no-bindings)
      nil
      bindings)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of bindings"
  (cond
    ((eq bindings fail) fail)
    ((variable-p pattern)
     (match-variable pattern input bindings))
    ((eql pattern input) bindings)
    ((segment-pattern-p pattern)
     (segment-match pattern input bindings))
    ((and (consp pattern) (consp input))
     (pat-match (rest pattern) (rest input)
        (pat-match (first pattern) (first input) bindings)))
    (t fail)))

(defun match-variable (var input bindings)
  "Does var match input? Updates and return bindings"
  (let ((binding (get-binding var bindings)))
    (cond
      ((not binding) (extend-bindings var input bindings))
      ((equal input (binding-val binding)) bindings)
      (t fail))))

(defun variable-p (x)
  "Is X a symbol starting with ?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun starts-with (list x)
  "Is this a list with first element x"
  (and (consp list) (eql (first list) x)))

(defun segment-pattern-p (pattern)
  "Is this a segment pattern ((?* var) . pat)"
  (and (consp pattern)
    (starts-with (first pattern) '?*)))

(defun segment-match (pattern input bindings &optional (start 0))
  "match segment pattern ((?* var) . pat) against input"
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
      (match-variable var input bindings)
      ; assume that pattern starts with a constant
      ; i.e two consecutive vars not allowed
      (let ((pos (position (first pat) input
                  :start start :test #'equal)))
        (if (null pos)
          fail
          (let ((b2 (pat-match pat (subseq input pos)
                     (match-variable var (subseq input 0 pos) bindings))))
              ; if this match failed try another longer one
              ; if it worked, check that variables match
              (if (eq b2 fail)
                (segment-match pattern input bindings (+ pos 1))
                (match-variable var (subseq input 0 pos) b2))))))))

(defvar *test-cases* '(
  ; pattern input expected_output
  ((i need a ?X)
   (i need a vacation)
   ((?X . VACATION)))

  ((i need a ?X)
   (i really need a vacation)
   nil)
  
  ((this is easy)
   (this is easy)
   ((T . T)))
  
  ((?X is ?X)
   ((2 + 2) is (2 + 2))
   ((?X 2 + 2)))

  ((?X is ?X) 
   ((2 + 2) is 4)
   nil)

  ((?P need . ?X)
   (i need a long vacation)
   ((?X A LONG VACATION) (?P . I )))

  (((?* ?p) need (?* ?x))
   (Mr Hulot and I need a vacation)
   ((?P MR HULOT AND I) (?X A VACATION)))
  
  (((?* ?x) a b (?* ?x))
   (1 2 a b a b 1 2 a b)
   ((?X 1 2 A B)))))

(defun test-driver ()
  "Run all test cases"
  (mapcar
    #'(lambda (test-case)
      (let* ((pattern (first test-case))
             (input (second test-case))
             (expected (third test-case))
             (actual (funcall #'pat-match pattern input)))
        (if (and (subsetp actual expected :test #'equal)
                 (subsetp expected actual :test #'equal))
          (format T "~a passed~%" pattern)
          (error "~a failed: expected ~a but got ~a~%"
            pattern expected actual))))
    *test-cases*))

(test-driver)