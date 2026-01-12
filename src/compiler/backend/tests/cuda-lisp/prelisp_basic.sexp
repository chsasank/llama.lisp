;; CMD: guile ../../prelisp.scm < {filename}
(prelisp
    (guile
        (define BM 64)
        (define BN 64)
        (define BK 8)
        (define TM 8)
        (define TN 8)
        (define ceil-div (lambda (M N)
            `(div (add ,M (sub ,N 1)) ,N))))
    (cuda-lisp
        (define-shared s [,(* TM TN) int])
        (define-kernel ((test void) (d (ptr int)) (n int))
            (declare m int)
            (declare n int)
            (declare k int)
            (set m ,BM)
            (set n ,TM)
            (set k ,(ceil-div 'm 'n))
            (ret))))
