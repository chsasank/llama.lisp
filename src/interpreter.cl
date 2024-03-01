; functional primitives
(defun trans (x)
  "Transposes a matrix"
  (apply #'mapcar #'list x))

(defun distl (x)
  "Distrbute from left
  (y (z1 z2 ... zn)) -> ((y z1) (y z2) ...)"
  (mapcar
    #'(lambda (zi) (cons (first x) (list zi)))
    (second x)))

(defun distr (x)
  "Distrbute from right
  ((y1 y2 .. yn) z) -> ((y1 z) (y2 z) ...)"
  (mapcar
    #'(lambda (yi) (cons yi (rest x)))
    (first x)))

(defun add (x)
  "Addition"
  (+ (first x) (second x)))

(defun mul (x)
  "multiplication"
  (* (first x) (second x)))

; functional forms
(defun alpha (fn)
  "Apply to all
  (alpha f): <x1 x2 ..> = <f:x1 f:x2 ..>"
  #'(lambda (x) (mapcar (fl fn) x)))

(defun idx (i)
  "Index data at i. Indexing starts at 0"
  #'(lambda (x) (nth i x)))

(defun cat (&rest fns)
  "Construction.
  (cat f1 .. fn): x = (f1:x  .. fn:x)"
  #'(lambda (x) 
      (mapcar
        #'(lambda (fn) (funcall (fl fn) x))
        fns)))

(defun comp (&rest fns)
  "composition of functions
  (comp f1 .. fn):x = f1:(f2:.. (fn: x))"
  #'(lambda (x)
      (reduce #'(lambda (f result) (funcall (fl f) result)) 
        fns :from-end T :initial-value x)))

(defun insert (fn)
  "insert function (/)
  /f:(x1, ..  xn) = f:(x1, /f1:(x2 .. xn))"
  (let ((fl-fn (fl fn)))
    #'(lambda (x)
        (reduce 
          #'(lambda (xi result)
              (funcall fl-fn (list xi result)))
          x :from-end T))))

; todo: onecharacter aliases

(defun get-fn (fn)
  ; TODO: work with env to define new functions
  (symbol-function fn))

(defun get-fn-form (fnf)
  ; TODO: work with env to define new functional forms
  (apply (symbol-function (first fnf)) (rest fnf)))

(defun fl (x)
  "Interpreter for llama lisp. FL stands for functional level"
  (cond
    ((symbolp x) (get-fn x))
    ((listp x) (get-fn-form x))
    (t (error "unkown expression ~a" x))))

; tests
(defvar *test-cases* '(
  ; function argument expected
  (trans
   ((1 2) (3 4))
   ((1 3) (2 4)))

  (distl
   (1 (3 4))
   ((1 3) (1 4)))

  (distl
   ((1 2) (3 4))
   (((1 2) 3) ((1 2) 4)))

  (distr
   ((1 2) 3)
   ((1 3) (2 3)))

  (distr
   ((1 2) (3 4))
   ((1 (3 4)) (2 (3 4))))

  ((alpha trans)
   (((1 2) (3 4))
    ((1 -2) (-3 4)))
   (((1 3) (2 4))
    ((1 -3) (-2 4))))

  ((idx 0)
   ((1 2) (3 4))
   (1 2))

  ((alpha (idx 1))
   ((1 2) (3 4))
   (2 4))

  ((cat (idx 1) (idx 0))
   ((1 2) (3 4))
    ((3 4) (1 2)))

  (add (1 4) 5)
  (mul (2 4) 8)

  ((comp add (alpha mul))
   ((2 3) (4 5))
   26)

  ((insert add)
   (1 2 3 4)
   10)

  ; inner product
  ((comp (insert add) (alpha mul) trans)
   ((1 2 3 4) (-1 2 3 -4))
   -4)
  
  ; matrix multiplication
  ((comp
      (alpha (alpha
        (comp (insert add) (alpha mul) trans)))
      (alpha distl)
      distr
      (cat (idx 0) (comp trans (idx 1))))
   (((1 2) (3 4))
    ((1 -2) (-3 4)))
   ((5 6) (-9 10)))))

(defun test-driver ()
  "Run all test cases"
  (mapcar
    #'(lambda (test-case)
      (let* ((fn (first test-case))
             (input (second test-case))
             (expected (third test-case))
             (actual (funcall (fl fn) input)))
        (if (equal actual expected)
          (format T "~a passed~%" fn)
          (format T "failed: expected ~a but got ~a~%"
            expected actual))))
    *test-cases*))

(test-driver)
