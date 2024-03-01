# Python Interoperability

Interoperability with python is very important for success of this project. Will use cl4py and py4cl or inspiration from them to create great experience for python developers.

## Install Quick Lisp

First, install quick lisp following instructions from here: https://lispcookbook.github.io/cl-cookbook/getting-started.html#install-quicklisp

```
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
```

In repl, do

```
(quicklisp-quickstart:install)
```

Exit repl and add this to `~/.sbclrc`

```common_lisp
#-quicklisp
  (let ((quicklisp-init (merge-pathnames
                          "quicklisp/setup.lisp"
                          (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
```

Open SBCL again and install a library and test it:

```common_lisp
(ql:quickload "str")
(str:title-case "HELLO LISP!")
```

## Import Python: Py4cl

Now install py4cl:

```common_lisp
(ql:quickload "py4cl")
```

Assuming you have `numpy` installed, try:

```common_lisp
(py4cl:import-module "numpy" :as "np")
(np:linspace 0.0 (* 2 pi) 20)
```

This should print

```
#(0.0 0.33069396 0.6613879 0.9920819 1.3227758 1.6534698 1.9841638 2.3148577
  2.6455517 2.9762456 3.3069396 3.6376336 3.9683275 4.2990217 4.6297154
  4.9604096 5.2911034 5.6217976 5.9524913 6.2831855)
```

And two dimensional arrays so:

```
(np:diag #(1 2 3 4))
```

```
#2A((1 0 0 0) (0 2 0 0) (0 0 3 0) (0 0 0 4))
```

## Export Lisp

Apparently I can export lisp functions to python using py4cl!

```common_lisp
(py4cl:python-exec "from scipy.integrate import romberg")

(py4cl:export-function (lambda (x) (/ (exp (- (* x x)))
                                      (sqrt pi))) "gaussian")

(py4cl:python-eval "romberg(gaussian, 0.0, 1.0)") ; => 0.4213504
```

Can I use these functions in another python file?

Does this mean I don't have to use cl4py? Can I use cl4py and py4cl both together? Won't there be circular import? Need to verify. It's amazing how both the libraries are very small.

py4cl library seems to be running python as a subprocess with lisp as parent while cl4py is doing the opposite. Ideally the experience I want is the experience of `pip install lisp`. Meaning, python should be the parent process and something like ecl should be packaged in python. But the experience I want seems to be closer to cl4py than py4cl because I will explicitly do `export` for the functions I care about.

Essentially I should modify cl4py to load python without a subprocess. But I am not sure how that would work. Need to explore more. Great layer and a great start.