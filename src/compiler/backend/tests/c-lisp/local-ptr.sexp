(c-lisp
    (define ((print int) (n int)))

    (define ((main void)) 
        (declare b int)
        (set b 5)

        (declare c (ptr int))       ;;array c
        (set c (alloc int 10))      ;; no of elements 10 

        (declare tmp (ptr int))
        (set tmp (ptradd c 0))  ;; point to position 0 and put 3
        (store tmp 3)
        (call print (load tmp) )
        (set tmp (ptradd c 1))  ;; point to position 1 and put 5
        (store tmp b)

        (call print (load tmp) )

        (ret)))