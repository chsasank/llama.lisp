(c-lisp
    ,@(include
        (time.h stdio.h) ; Headers
        (asctime gmtime) ; Functions
        (tm)) ; Structs

    (define ((print int) (n int)))

    (define ((main void))
        (declare time (ptr (struct tm)))
        (declare seconds int64) (set seconds (sext 0 int64))
        (set time (call gmtime (ptr-to seconds)))
        (call print (load (sptradd time field-5)))))
