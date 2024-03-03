; functional primitives
(defun add ()
  "code gen for addition"
  ; we assume everywhere that `inp` is the variable
  ; defined 'earlier'
  '(+ (first inp) (second inp)))

(defun mul ()
  "code gen for mul"
  '(* (first inp) (second inp)))

(defun distl ()
  "distribute from left
  in == (y (z1 z2 ... zn)) -> ((y z1) (y z2) ...)"
  '(let ((y (first inp)))
      (loop for zi in (second inp) collect
        (list y zi))))

(defun distr ()
  "distribute from right
  in == ((y1 y2 .. yn) z) -> ((y1 z) (y2 z) ...)"
  '(let ((z (second inp)))
      (loop for yi in (first inp) collect
        (list yi z))))

(defun trans()
  "Transpose a matrix
  in == ((x11 .. xm1) .. (xn1 .. xmn)) ->
    (x11 .. x1m) .. (x1n .. xnm))
  "
  '(let ((m (length (first inp))))
    (loop for i from 0 below m collect
      (loop for row in inp collect
        (nth i row)))))

(defun alpha (fn)
  "Code gen for alpha.
  (alpha f): <x1 x2 ..> = <f:x1 f:x2 ..>"
  `(loop for xi in inp collect
    (let ((inp xi)) ,(code-gen fn))))

(defun idx (i)
  "code gen for index"
  `(nth ,i inp))

(defun cat (&rest fns)
  "code gen for construction"
  `(list ,@(loop for fn in fns collect
    (code-gen fn))))

(defun get-fn (fn)
  ; TODO: work with env to define new functions
  (symbol-function fn))

(defun get-fn-form (fnf)
  ; TODO: work with env to define new functional forms
  (apply (symbol-function (first fnf)) (rest fnf)))

; compiler
(defun code-gen(x)
  "Compiler for llama lisp. FL stands for function level."
  (cond
    ((symbolp x) (funcall (get-fn x)))
    ((listp x) (get-fn-form x))
    (t (error "unkown expression ~a" (type-of x)))))

; runtime
(defun fl (x)
  (eval `(lambda (inp) ,(code-gen x))))

; tests
(defvar *test-cases* '(
  ; function argument expected
  (add (1 4) 5)
  (mul (2 4) 8)

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
  
  (trans
   ((1 2) (3 4))
   ((1 3) (2 4)))

  (trans
   ((1 2 -1) (3 4 -2))
   ((1 3) (2 4) (-1 -2)))

  ((alpha add)
   ((1 2) (3 4))
   (3 7))

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

  ((cat add mul)
   (3 4)
   (7 12))

  ((cat (idx 1) (idx 0))
   ((1 2) (3 4))
    ((3 4) (1 2)))
))

; test driver
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