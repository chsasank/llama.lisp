(c-lisp
    ,@(include
        (stdio.h stdlib.h string.h) ; Headers
        (malloc puts strcpy)) ; Functions to include

    (define ((main void))
        (declare buf (ptr int8))
        (set buf (call malloc (sext 50 int64)))
        (call strcpy buf "Brought to you by malloc, strcpy and puts")
        (call puts buf)))
