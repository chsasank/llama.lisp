# Syntactic Sugar (or Macros)

Since we have somewhat of a base language implemented both as a compiler and interpreter, I would like to make my life simpler by introducing automatic fl code generators for some common constructs.

## Shortcuts/names for expressions

Most useful construct being defining shortcuts so that we don't have to write the full name. We just substitute names of functions with the corresponding body in the list that represents FL programs. Because we're in lisp, these kind of things are a breeze!

We just store the mapping from function names to their expression in a simple a-list (i,e list of pairs).

```common_lisp
(defvar *fl-env* nil
  "A list of all functions avialable for rewrite.")

(defun fl-let (fn-name body)
  "Add fl function to *fl-env"
  (if (assoc fn-name  *fl-env*)
    (setf (second (assoc fn-name *fl-env*)) body)
    (push (cons fn-name body) *fl-env*)))
```

We just substitute the names recursively to expand them:

```common_lisp
(defun expand-fl-shortcuts (fl-x)
  "Recursively expand all the shortcuts used in the body"
  (cond
    ((symbolp fl-x) 
      (let ((shortcut (get-fl-shortcut fl-x)))
        (if shortcut (expand-fl-shortcuts shortcut) fl-x)))
    ((listp fl-x) (mapcar #'expand-fl-shortcuts fl-x))
    (t fl-x)))
```

I created a convenience function `fl-let` that is alias of add-fl-shortcut


## Structure operator

This was more complicated. Given a shape of expected input, we need to create identity function using `for-loops`. This is the basis of much [optimization](./optimizer.md) and a starting of type system. This function simply generates lists like so:

```common_lisp
* (structure-operator '(? 4 ?))
(FOR-LOOP #:|i120| (CONST 0) LEN
 (FOR-LOOP #:|i121| (CONST 0) 4
  (FOR-LOOP #:|i122| (CONST 0) (COMP LEN (IDX 0) (IDX 0))
   (COMP (IDX #:|i122|) (IDX #:|i121|) (IDX #:|i120|)))))

* (structure-operator '(? ?))
(FOR-LOOP #:|i123| (CONST 0) LEN
 (FOR-LOOP #:|i124| (CONST 0) (COMP LEN (IDX 0))
  (COMP (IDX #:|i124|) (IDX #:|i123|))))
```

Note how this create `for-loop`s with lengths based on shape if given. Also note the creation of random symbols for loop variables.