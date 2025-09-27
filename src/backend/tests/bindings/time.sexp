(c-lisp
    ,@(include
        (time.h stdio.h) ; Headers
        (asctime gmtime) ; Functions
        (tm) ; Structs
        (time_t)) ; Typedefs

    (define ((print int) (n int)))

    (define ((main void))
        (declare time (ptr (struct tm)))
        (declare seconds ,time_t) (set seconds (sext 0 int64))
        (set time (call gmtime (ptr-to seconds)))
        (call print (load (sptradd time tm_year)))))
