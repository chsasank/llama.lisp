# Optimization

So the reason the language is based on FL is because of amazing algebra of programs[1]. There are some rules on how we can correctly transform a program. We can estimate the cost of a given program and then try to optimize it by applying these rules. This is not too different from playing Chess (although there's only single player). There is a wide body of literature available on how to apply rules and achieve an objective. Many chapters from PAIP book come to mind[2].

## Additional Forms

I describe the new forms and techniques introduced to help optimize programs, but in the original notation.

### Loop Functional Form

In his original Turing lecture [1], Backus hinted that rules can be applied to optimize the programs. In a later paper[3], he explicitly described these optimizations with examples of matrix multiplication. He introduced a new functional form which obviously correlates to a Fortran loop. It looks like this:

```
[E(i) i = f, g]
```

The meaning of this form can be described as 

```
[E(i) i = f, g]: x = [E(f:x), E(f:x + 1) .. E(g:x)]:x
```

This is clearly described in Fortran as

```
for i = f:x, g:x
    r[i] := p(E(i), x)
```

Where `p(E(i), x)` is a program that computes `E(i):x`

As an example, consider

```
[i i=~1, len]: <a, b, c> = [1, 2, 3]: <a, b c> = <a, b, c>
```

Thus, `[i i=~1, len]` is a identity function for lists.

We can also have nested loop forms. Thus,

```
[[i.j i=~1, len.1] j=~1,len]
```

is identity function for matrices

### A Type System

To optimize a program, it helps to know the type of the input. This also makes algebraic optimization straight forward based on specific rules. The way type system works is to create a identity function (called structure operator by backus) that is identity (like above) for valid inputs. You then apply transformation rules to optimize the program.

For example, take inner product

```
def IP = \+.@*.trans
```

Since we expect pair of equal length sequences, our 'type' is described by the following function:

```
C = [[i.j i=~1,len.1] j=~1,~2]
```

This is identity for inputs `<<x1, .. xn> <y1 .. yn>>`. Therefore, 

```
IP = IP.C
= \+.@*.trans.[[i.j i=~1,len.1] j=~1,~2]
= \+.@*.[[i.j j=~1,~2] i=~1,len.1]   (applied the rule from rulebook)
= \+.[*.[i.j j=~1,~2] i=~1,len.1]
= \+.[*.[i.1, j.2] i=~1,len.1]
```

Which can then be transformed into Fortran program

```
r = 0
for i = 1, len:a
    r = r + (a[i] * b[i])
```

## Optimization

Let's get started on optimization. I have a list of rules something like this:

```
(comp (alpha f) (for-loop E g h)) ==
(for-loop (comp f E) g h)
```

This is my rule. How am I gonna represent it? I will take inspiration and code from Chapter 5: Eliza. It simply does what I am looking for:
1. Find relevant rules to apply to present conversation
2. Apply one of the rules
3. Repeat

And then we will apply search on this. We'll come back to it later. First, let's implement pattern matching. My code from this chapter is in [here](https://github.com/chsasank/paip/blob/main/ch6/pattern-matching.cl).

I implemented pattern matching and rule system. 

### `comp`

So having comp made things more complicated for rewrites because `(comp x* y z*) = (comp (comp x*) y (comp z*))` where `x*` and `z*` represent multiple times. While I can write a rule like that, it is quite inefficient and my matcher can't do the reverse. So I am just writing it manually.

Besides, the pattern matcher used so far was not good enough anymore. So I moved the code to more improved pattern matcher from Chapter 6.

### Recursive rules

Any rule I wrote can be matched in any part of subtree.

References:
1. Backus Turing
2. PAip
3. https://link.springer.com/chapter/10.1007/3-540-15198-2_5