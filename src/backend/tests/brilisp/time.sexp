(brilisp
    (define-struct tm  ;; Taken from C standard library
        (int        ;;  int         tm_sec;    /* Seconds          [0, 60] */
         int        ;;  int         tm_min;    /* Minutes          [0, 59] */
         int        ;;  int         tm_hour;   /* Hour             [0, 23] */
         int        ;;  int         tm_mday;   /* Day of the month [1, 31] */
         int        ;;  int         tm_mon;    /* Month            [0, 11]  (January = 0) */
         int        ;;  int         tm_year;   /* Year minus 1900 */
         int        ;;  int         tm_wday;   /* Day of the week  [0, 6]   (Sunday = 0) */
         int        ;;  int         tm_yday;   /* Day of the year  [0, 365] (Jan/01 = 0) */
         int        ;;  int         tm_isdst;  /* Daylight savings flag */

         int int    ;;  long        tm_gmtoff; /* Seconds East of UTC */
         (ptr int)) ;;  const char *tm_zone;   /* Timezone abbreviation */
    )

    (define ((asctime (ptr int)) (t (ptr tm))))
    (define ((printf void) (s (ptr int))))

    (define ((set-date void) (time (ptr tm)) (day int) (month int) (year int) (daynum int))
        (set (day-p (ptr int)) (ptradd time 0 3))
        (store day-p day)
        (set (month-p (ptr int)) (ptradd time 0 4))
        (store month-p month)
        (set (year-p (ptr int)) (ptradd time 0 5))
        (set (base int) (const 1900))
        (set (year int) (sub year base))
        (store year-p year)
        (set (daynum-p (ptr int)) (ptradd time 0 6))
        (store daynum-p daynum)
        (ret))

    (define ((set-to-midnight void) (time (ptr tm)))
        (set (zero int) (const 0))
        (set (sec-p (ptr int)) (ptradd time 0 0))
        (store sec-p zero)
        (set (min-p (ptr int)) (ptradd time 0 1))
        (store min-p zero)
        (set (hour-p (ptr int)) (ptradd time 0 2))
        (store hour-p zero)
        (set (gmtoff-p (ptr int)) (ptradd time 0 9))
        (store gmtoff-p zero)
        (set (gmtoff-p (ptr int)) (ptradd time 0 10))
        (store gmtoff-p zero)
        (ret))

    (define ((main void))
        (set (one int) (const 1))
        (set (tmp-p (ptr int)) (alloc one))
        (set (time-struct (ptr tm)) (alloc one))

        (set (d int) (const 22))
        (set (m int) (const 6))
        (set (y int) (const 2024))
        (set (dn int) (const 6))

        (set (tmp void) (call set-date time-struct d m y dn))
        (set (tmp void) (call set-to-midnight time-struct))

        (set (tmp (ptr int)) (call asctime time-struct))
        (set (tmp void) (call printf tmp))
        (ret)))
