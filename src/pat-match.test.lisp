(load "pat-match.lisp")

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
   ((?X 1 2 A B)))

  ((x = (?is ?n numberp)) 
   (x = 34)
   ((?n . 34)))

  ((x = (?is ?n numberp)) 
   (x = x)
   nil)

  ((?x (?or < = >) ?y)
   (3 < 4)
   ((?Y . 4) (?X . 3)))

  ((x = (?and (?is ?n numberp) (?is ?n oddp)))
   (x = 3)
   ((?N . 3)))
  
  ((?x /= (?not ?x))
   (3 /= 4)
   ((?X . 3)))
  
  ((?x > ?y (?if (> ?x ?y)))
   (4 > 3)
   ((?Y . 3) (?X . 4)))

  ((a (?* ?x) d)
   (a b c d)
   ((?X B C)))
  
  ((a (?* ?x) (?* ?y) d)
   (a b c d)
   ((?Y B C) (?X)))

  ((a (?* ?x) (?* ?y) ?x ?y)
   (a b c d (b c) (d))
   ((?Y D) (?X B C)))))

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