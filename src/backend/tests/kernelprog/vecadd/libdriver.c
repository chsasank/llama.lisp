#include <stdio.h>
#include <stdlib.h>

/* Temporary support library for vecadd.sexp.
   Ideally, and eventually, this file shouldn't exist */

/* Constants that cannot (yet) be defined in C-Lisp. */
char * ReadingStdinMsg = "Reading kernel from standard input...";
char * reading_stdin_msg () {
    return ReadingStdinMsg;
}
char * KernelNameStr = "kernel";
char * kernel_name_str () {
    return KernelNameStr;
}
char * ErrorStatusMsg = "Non-zero return status";
char * error_status_msg () {
    return ErrorStatusMsg;
}
char * MaxErrMsg = "Max error:";
char * max_err_msg () {
    return MaxErrMsg;
}
char eof_char () {
    return EOF;
}
char null_char () {
    return '\0';
}
void * null_ptr () {
    return NULL;
}
int64_t big_zero () {
    return 0;
}

/* Barebones output support */
void print (int n) {
    printf("%d\n", n);
}
void fprint (float n) {
    printf ("%f\n", n);
}
