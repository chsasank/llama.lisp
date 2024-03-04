# Compiler and Runtime

This language needs to feel like interpreted language when it really is a compiled language. That is how SBCL works as well. So JIT compilation is an important design goal.

## LLVM Lite?

Because I have figured out using python libraries [seamlessly](./python-interop.md) in common lisp, should I just use `llvmlite` library from numba[1]? It has a good runtime *and* llvm builder API. It generates LLVM as a text file meaning I can actually move from it to common lisp based generator once mature.

Let's look at number of lines builder API for llvmlite has. Alright it's a lot of lines: builder.py (the interface) alone has > 1000 lines! Not worth re-implementing it from scratch right now. If object API works fine through cl4py then I consider that a win! Let's check documentation of py4cl to verify on how python objects are transformed in common lisp.

Let's roll with cl4py and llvmlite based builder. Will move to different generator later. Let's do a quick python tutorial of LLVM lite. Gone through the tutorial and boy LLVM is very complicated. I want simple stuff.

## Let's keep it Simple

Earlier I have written a [scheme compiler](https://github.com/chsasank/scheme-incremental-compiler/) that actually generates x86 code. It is surprisingly easy to do that. You'll have to follow some C ABI rules but it's surprisingly easy. I used stack to maintain variables which will be in memory and therefore not the best register allocation, but it got the job done.

I want to keep things simple and understandable. I know that for the problem I am solving compiler optimizations that are there in GCC or LLVM doesn't matter at all to me [2]. So I am not losing much by renouncing those optimizations and working on things that really matter to me.

In fact even x86 generation is too complicated and low level right now because of runtime constraints. So I'll work on compiling to common lisp just like I have done in Prolog compiler in Norvig's book[3]. This lower level is sort of like 'virtual machine' or 'intermediate representation' or whatever you want to call. For the first cut, this IR will be interpreted and my compiler basically translates my language to this IR. This is how we bootstrap a multi level compiler.

## Register Allocation

My sense says that this is one of the most important considerations of my system because I need to balance memory and take advantage of memory hierarchy. That is the basis of GotoBLAS algorithm. It seems wasteful to generate registers at ultra low semantic level instead of actually doing it at higher semantic levels. Note that in our language, we have no variables names. A way of thinking about register allocation is assigning names to these variables but with a given budget of space.

Let's design a basic register allocation mechanism for this:

```
(comp (insert add) (alpha mul) trans)
```

I would like to read the original reference on register allocation. I know of this paper about red-blue ball game [3] that tries to find optimal allocation. A good reference/overview can be found in these slides[4].

## Immediate Goal: Compile to Lisp

Let's get started on a compiler. Our goal is 'inline' everything into one giant function in common lisp. We will generate this in common lisp using the homoiconicity property of it[5]. Since a program itself is a list in common lisp, our problem statement is to simply construct a list. Our aim to generate single function is valid because we don't definitions yet - neither for variables nor for functions. Let's use the PAIP book as reference. Relevant sections include 

1. Section 9.2: [Compiling one language to another](https://github.com/norvig/paip-lisp/blob/main/docs/chapter9.md#92-compiling-one-language-into-another)
2. Chapter 12: [Compiling logic programs](https://github.com/norvig/paip-lisp/blob/main/docs/chapter12.md#121-a-prolog-compiler)

Let's take the example program from Section 9.2 and get inspired by it. Here is a compiler for a generator of BNF grammar (an interpreter is found in Section 2.3)

```common_lisp
; helper functions
(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (seq)
  "Pick a random element out of a sequence."
  (elt seq (random (length seq))))

; compile the rules
(defun compile-rule (rule)
    "Translate a grammar rule into a LISP function definition"
    (let ((rhs (rule-rhs rule)))
        `(defun ,(rule-lhs rule) ()
            ,(cond 
                ((every #'atom rhs) `(one-of ',rhs))
                ((length=1 rhs) (build-code (first rhs)))
                (t `(case (random ,(length rhs))
                        ,@(build-cases 0 rhs)))))))

(defun build-cases (number choices)
    "Return a list of case-clauses"
    (when choices
        (cons (list number (build-code (first choices)))
            (build-cases (+ number 1) (rest choices)))))

(defun build-code (choice)
    "Append together multiple constituents"
    (cond 
        ((null choice) nil)
        ((atom choice) (list choice))
        ((length=1 choice) choice)
        (t `(append ,@(mapcar #'build-code choice)))))

(defun length=1 (x)
    "Is x a list of length 1"
    (and (consp x) (null (cdr x))))

; english grammar
(compile-rule '(Sentence -> (NP VP)))
; ==> (DEFUN SENTENCE () (APPEND (NP) (VP)))
```

In this code the most important common lisp construct is somewhat similar to string formatting in python:

```
$ python
>>> a = "alice"
>>> f"{a} says hello world"
'alice says hello world'
```

In common lisp, we can use backtick (`), comma (,) to make list construction easy:

```
$ sbcl
* (setf a 'alice)
ALICE
* `(,a says hello world)
(ALICE SAYS HELLO WORLD)
```

Note that in lisp version, we used symbols instead of strings. Another convenience we have is `,@` to make appending to lists easy

```
* (setf ab '(alice and bob))
(ALICE AND BOB)
* `(,ab says hello world)
((ALICE AND BOB) SAYS HELLO WORLD)
* `(,@ab says hello world)
(ALICE AND BOB SAYS HELLO WORLD)
```

This should get us started on the path of lowering.

## Intermediate language

For the sake of sanity, let's keep track of the constructs we actually emit during the generation of one single giant function. Let's think critically the constructs we use and why.

### Loops

Let's not use `map` or `lambda` for the time-being and stick to simple `loop` construct in CL. Here's how it works

```
* (loop for x in '(1 2 3) collect
    (* x 10))
(10 20 30)
```

Why I use loop is because ultimately we have to transform our language into something that runs on von Neumann machines. Implementing tail recursion like in scheme is an option but that's a bit of work and doesn't translate nicely enough to lower level. If we keep on seeing similar loop construct being used everywhere, let's macro it as well.

### Names: Input

In my code gen, I assumed `in` means input to a code-gen. For example, check this out:

```common_lisp
(defun add ()
  "code gen for addition"
  ; we assume everywhere that `x` is the variable
  ; defined 'earlier'
  '(+ (first in) (second in)))
```

Note how `in` is not an input parameter to `add` function itself. Code-gen assumes `in` is defined 'earlier' whatever that means. Our code-gen and runtime looks like this:

```common_lisp
; compiler
(defun code-gen(x)
  "Compiler for llama lisp. FL stands for function level."
  (cond
    ((symbolp x)
      (funcall (get-fn x)))
    (t (error "unkown expression ~a" (type-of x)))))

; runtime
(defun fl (x)
  (eval `(lambda (in) ,(code-gen x))))
```

code-gen creates lisp code and `eval` evaluates it into a lambda where `in` is a parameter. And that's how `in` is evaluated in above `add`.

### Names: Intermediate Variables

I just used a `let` for naming a variable beyond this `in` parameter. Specifically check this function:

```common_lisp
(defun distl ()
  "distribute from left
  in == (y (z1 z2 ... zn)) -> ((y z1) (y z2) ...)"
  '(let ((y (first in)))
      (loop for zi in (second in) collect
        (list y zi))))
```

I could have written it as

```common_lisp
(defun distl ()
  (loop for zi in (second in) collect
    (list (first in) zi)))
```

But this would have computed `(first in)` over and over. That seemed wasteful and that's why I 'named' it. This is a perfect example of 'register allocation' as I called it earlier. I 'cached' the results so that I don't have to recompute them. Could this caching me done [automatically](https://github.com/norvig/paip-lisp/blob/main/docs/chapter9.md#91-caching-results-of-previous-computations-memoization)?

### Loop: Range and Indexing

For implementing transpose, I unfortunately had to use more constructs. Specifically I had to use loops with index and range like in python implementation. I think I can replace earlier loops with range and index later may be instead of just variables. I also used the function `length`. No matter, I don't like my implementation of `trasn` here:

```common_lisp
(defun trans()
  "Transpose a matrix
  in == ((x11 .. xm1) .. (xn1 .. xmn)) ->
    (x11 .. x1m) .. (x1n .. xnm))
  "
  '(let ((m (length (first in))))
    (loop for i from 0 below m collect
      (loop for row in in collect
        (nth i row)))))
```

### Naming: Renaming

Now on to implementing the functional forms and here's where fun starts. To implement `alpha`, I needed to implement renaming/scope whatever you wanna call. This is a bit ugly but it works. I renamed `in` to `inp` because it gets confusing with `in` of `for`

```common_lisp
(defun alpha (fn)
  "Code gen for alpha.
  (alpha f): <x1 x2 ..> = <f:x1 f:x2 ..>"
  `(loop for xi in inp
    (let ((inp xi))
      (collect (code-gen x)))))
```

This renaming is sort of core of what lambda or at least how I implemented it in scheme.

### Naming: Setting

This is the hardest pill to swallow because this means I have side effects or something like that. I needed to do this for `comp`. I can do 'SSA' and create temporary variables for each of the steps, but I feel I will loose the semantics later when I optimize. So here's what I did:

```common_lisp
(defun comp (&rest fns)
  "composition"
  `(let ((inp inp))
      ,@(loop for fn in (reverse fns) collect
          (list 'setf 'inp (code-gen fn)))))
```

Note that I have had to do let at the start because I needed to rename. I feel this is clearer construct to optimize later than when with simple SSA. If I do SSA on this, I should be able to do linear scan and assign same register to all, but I am not very comfortable with that.

## Bootstrapping the compiler

It would be so nice if I could implement the compiler directly in our language and use previously written interpreter to bootstrap. May be with that I can avoid naming variables and writing loops. Instead of naming variables I will need to name functions.

For example, here's a recursive implementation of factorial (in the original notation)

```
def ! = eq0 -> ~1, x.[id, !.sub1]
def eq0 = eq . [id, ~0]
def sub1 = -.[id, ~1]
```

I have not yet implemented definitions (i.e environment), conditionals and other functions used here. But in general, bootstrapping is a good test for usability of the language.

Consider generating C Code: https://github.com/kiselgra/c-mera

References:

1. Numba todo
2. Private correspondence on compiler group - document for later.
3. H T Kung paper todo.
4. https://web.stanford.edu/class/archive/cs/cs143/cs143.1128/lectures/17/Slides17.pdf