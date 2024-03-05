# Discussion on Additional Forms 

## `for-loop` Form Syntax

See [optimizer.md](./optimizer.md) for more information on this. I need a syntax to represent for-loop.

Original syntax used by Backus

```
[E(i) i = f, g]
```

My current syntax of this

```
(for-loop E f g)
```

Note how I excluded `i`. This is a problem because many rules require manipulation of `i`. Implementation of this as a compiler also has been a challenge. Interpreter was relatively easier:

```common_lisp
(defun for-loop (body-form start end)
  "Loop form
    [E(i) i = f, g]: x = [E(f:x), E(f:x + 1) .. E(g:x)]:x
    E = body-form, f = start, g = end"
  #'(lambda (x)
      (loop for idx
          from (funcall (fl start) x)
          below (funcall (fl end) x)
        collect (funcall (fl (list body-form idx)) x))))
```

I needed start and end at the runtime to generate the loop and that made compiler a bit painful. Let's get back to matter of syntax. Most relevant line in the above is following:

```
(funcall (fl (list body-form idx)) x)
```

I am compiling `(E i)` (using notation of backus `[E(i) i = f, g]`) using fl and that is exactly how this form is intended to be implemented by Backus:

```
for i = f:x, g:x
    r[i] := p(E(i), x)
```

Where `p(E(i), x)` corresponds to the above highlighted line from my code. Ok then what's the problem?

I need to represent following rule from Backus in my syntax:

```
trans.[[E(i,j) i=f,g] j=r,s] = [[E(i,j) j=r,s] i=f,g]
```

Here the problem comes down to `E(i, j)`. Backus simply represented it as `E(i, j)`, how does it work for me? Let's examine specifically:

```
[[E(i,j) i=f,g] j=r,s]
```

In my current construct this is:

```
(for-loop (for-loop E f g) r s)
```

What are the semantics of this?

```
(loop for i from r:x below s:x collect
    (funcall (fl (list (for-loop E f g) i)) x)
)
```

I need to be able to evaluate

```
((for-loop E f g) i)
```

That is for-loop in which i is constant. This will probably need me to use lambdas, closures and stuff.

Ok, let's try this:

```
(for-loop
    (lambda (i) body-form)
     f g)
```

`(lambda (i) body-form)` generates FL code with i as constant. I specifically named the variable i and that makes all the difference - because I can substitute it later. Also the functional form actually gets converted to FL code. Sort of like a macro - not actual functional form.

In a sense, this is what `(list body-form idx)` is doing but it is sort of implicit. Let's try to write identity function in this new formalism:

```
; before
(for-loop idx (const 0) len)

; now
(for-loop #'(lambda (i) `(idx i))
    (const 0)
    len)
```

This makes a lot of sense. Let's go back to our problem and see if this works:

```
; before
(for-loop (for-loop E f g) r s)

; after
(for-loop #'(lambda (j) 
    `(for-loop #(lambda (i)
        ?
    ) f g
    )
    r s))
```

This is a bit confusing because FL programs contains a small part of lisp inside it. Let's take concrete example of identity for matrices:

```
[[i.j i=~1, len.1] j=~1, len]
```

Let's just translate it directly


```
(for-loop
    (for-loop (comp (idx i) (idx j))
        i (const 1) (comp len (idx 1)))
    j (const 1) len)
```

Let's make it more like C by rearranging numbers:

```
(for-loop j (const 0) len
    (for-loop i (const 0) (comp len (idx 0))
        (comp (idx i) (idx j))))
```

Very nice! Let's just implement this lol. I have a feeling that this construct is related to recursion but that's for later. How to write interpreter for this? Just use `sublis` lol. This works!

Later I will figure out how to write a compiler. May be then I can upgrade to emitting C.

## `reduce` construct

Insert along with for-loop can be an useful thing.