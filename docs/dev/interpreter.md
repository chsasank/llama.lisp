# Interpreter

## Python Implementation

I have written Python based interpreter to test out the ideas behind Llama lisp. Here is it in < 100 lines:


```python
def trans(X):
    return [
        [X[n][m] for n in range(len(X))]
        for m in range(len(X[0]))
    ]

def distl(X):
    assert len(X) == 2
    return [[X[0], z] for z in X[1]]

def distr(X):
    assert len(X) == 2
    return [[y, X[1]] for y in X[0]]

def alpha(fn):
    return lambda X: [fn(x) for x in X]

def idx_0(X):
    return X[0]

def idx_1(X):
    return X[1]

def cat(*fns):
    return lambda X: [fn(X) for fn in fns]

def mul(X):
    return X[0] * X[1]

def add(X):
    return X[0] + X[1]

def comp(*fn_list):
    return lambda X: fn_list[0](X) if len(fn_list) == 1 else comp(*fn_list[:-1])(fn_list[-1](X))

def insert(f):
    return lambda X: X[0] if len(X) == 1 else f([X[0], insert(f)(X[1:])])


## Inner Product
IP = comp(
    insert(add),
    alpha(mul),
    trans
)
MM = comp(
    alpha(alpha (IP)),
    alpha(distl),
    distr,
    cat(idx_0, comp(trans, idx_1))
)

# 4x4
mat = [[0, 1, 2, 3],
       [-1, 0, 1, 2],
       [-2, -1, 0, 1],
       [-3, -2, -1, 0]]
print(MM([mat, mat]))
# [[-14, -8, -2, 4],
#  [-8, -6, -4, -2],
#  [-2, -4, -6, -8],
# [4, -2, -8, -14]]
```

## Common Lisp Implementation

That should set the context of my common lisp implementation to do the same. I should use names from FL paper.

The constructs in python I used for this:

1. List comprehensions
2. Range
3. Indexing
4. Conditional expressions
5. Lambda

I kept these constructs minimal so that I can move to lisp fairly easily.

### How to model lists?

This is the first decision point. Should I just use lists like I did in python? Or move to vectors? Let's see what [py4cl uses for numpy](./python-interop.md):

```commonlisp
* (ql:quickload :py4cl)
* (py4cl:import-module "numpy" :as "np")
* (type-of (np:linspace 0.0 (* 2 pi) 20))
(SIMPLE-VECTOR 20)
* (type-of (np:diag #(1 2 3 4)))
(SIMPLE-ARRAY T (4 4))
```

So what is simple array? Documentation can be found [here](https://lispcookbook.github.io/cl-cookbook/arrays.html). Another way to create arrays is:

```
(defparameter *a* (make-array '(3 2) :initial-element 1.0))
(aref *a* 0 1)
```

Ok this is good enough for me. Let's use simple arrays. Let's write simple matmul in CL:

```commonlisp

(defun matrix-multiply (matrix-a matrix-b)
  (let* ((m (array-dimension matrix-a 0))
         (n (array-dimension matrix-b 1))
         (k (array-dimension matrix-b 0))
         (matrix-c (make-array (list m n) :initial-element 0)))
    (dotimes (i m)
      (dotimes (j n)
        (dotimes (p k)
          (incf (aref matrix-c i j)
            (* (aref matrix-a i p) (aref matrix-b p j))))))
    matrix-c))

(ql:quickload :py4cl)
(py4cl:import-module "numpy" :as "np")
(matrix-multiply (np:diag #(1 2 3 4)) (np:diag #(4 3 2 1)))
```

Should print

```
#2A((4 0 0 0) (0 6 0 0) (0 0 6 0) (0 0 0 4))
```