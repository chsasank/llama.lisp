;; Multi-output inline asm returning a struct, destructured with extractvalue.
(c-lisp
  (define-struct pair (lo int) (hi int))
  (define ((print int) (n int)))

  (define-inline-brilisp ((get_pair void) (out (ptr int)))
    (set (p (struct pair))
         (asm "movl $$42, $0; movl $$43, $1;" "=r,=r"))
    (set (v0 int) (extractvalue p 0))
    (set (v1 int) (extractvalue p 1))
    (set (out1 (ptr int)) (ptradd out 1))
    (store out v0)
    (store out1 v1)
    (ret))

  (define ((main void) (argc int) (argv (ptr (ptr int))))
    (declare out-ptr (ptr int))
    (set out-ptr (alloc int 2))
    (call get_pair out-ptr)
    (call print (load (ptradd out-ptr 0)))
    (call print (load (ptradd out-ptr 1)))
    (ret)))
