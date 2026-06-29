;; Unsigned vs signed comparisons on negative operands.
(c-lisp
    (define ((print int) (n int)))

    (define ((main void) (argc int) (argv (ptr (ptr int))))
        (declare n int)
        (set n -1)

        ;; Signed comparisons treat -1 as less than 1.
        (call print (select (lt n 1) 1 0))
        (call print (select (gt n 1) 1 0))

        ;; Unsigned comparisons treat -1 as 0xFFFFFFFF, greater than 1.
        (call print (select (ult n 1) 1 0))
        (call print (select (ugt n 1) 1 0))
        (call print (select (ule n 1) 1 0))
        (call print (select (uge n 1) 1 0))

        ;; ule and uge reflexivity.
        (call print (select (ule n n) 1 0))
        (call print (select (uge n n) 1 0))

        (ret)))
