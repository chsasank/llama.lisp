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

## Install Py4cl

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