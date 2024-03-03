; functional primitives
(defun add ()
  "code gen for addition"
  ; we assume everywhere that `x` is the variable
  ; defined 'earlier'
  '(+ (first in) (second in)))

(defun mul ()
  "code gen for mul"
  '(* (first in) (second in)))

(defun distl ()
  "distribute from left
  in == (y (z1 z2 ... zn)) -> ((y z1) (y z2) ...)"
  '(let ((y (first in)))
      (loop for zi in (second in)
        collect (list y zi))))

(defun distr ()
  "distribute from right
  in == ((y1 y2 .. yn) z) -> ((y1 z) (y2 z) ...)"
  '(let ((z (second in)))
      (loop for yi in (first in)
        collect (list yi z))))

(defun get-fn (fn)
  ; TODO: work with env to define new functions
  (symbol-function fn))

; compiler
(defun code-gen(x)
  "Compiler for llama lisp. FL stands for function level."
  (cond
    ((symbolp x)
      (funcall (get-fn x)))
    (t (error "unkown expression ~a" (type-of x)))))

; runtime
(defun fl (x)
  (eval `(lambda (in) ,(code-gen x))))

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