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

I would like to read the original reference on register allocation. I know of this paper about red-blue ball game [3] that tries to find optimal allocation. 

References:

1. Numba todo
2. Private correspondence on compiler group - document for later.
3. H T Kung paper todo.