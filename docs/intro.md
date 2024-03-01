# Introduction to llama lisp

Llama lisp is a language for writing high performance computing (HPC) kernels. It is a dialect of Common Lisp (CL). CL is a very popular language in reasoning, symbolic computing and originally AI. Common lisp have some of the best implementations including SBCL and ECL which perform close to C language[1]! At the same time, it's a very high level dynamic language. This makes it perfect for someone like me who want *both* high performance and high usability. 

The main feature of Llama lisp is the ability to write kernels like matrix multiplication or convolution. These kernels are really hard to get right and require a lot of hand optimization. Our aim is to eliminate this and make kernel programming easy.

Llama lisp is highly inspired by FL language[2] by John Backus, inventor of Fortran language. Fortran is arguably the first programming language. It is designed for numerical computation and still is a mainstay in HPC world. John Backus in his Turing lecture[3] criticized Fortran for being imperative and having introduced what he calls as Von-Neumann bottleneck to the programming languages.

His proposed alternative to imperative languages was so-called functional level (FL) language. In the language, there are no names for variables and the programs are basically a pipeline of functions. In FL, there are higher-order functions that take in functions as input like `map` and emit functions. Note that this style of programming is quite different from what we normally call functional programming (aka lambda calculus).

A very attractive feature of FL language is algebra of programs. Backus creates algebraic laws for equivalence of programs -- not unlike high school algebra. Imagine 'solving' the equation `(x+y)^2`: you will apply commutative and distributive laws of addition and multiplication to obtain `x^2 + 2*x*y + y^2`. FL's program algebraic laws play the same role as these but applied on functions instead of objects. This allows me to optimize programs like playing a chess game.

Another inspiration is BLIS library which is a 'refactor' of goto algorithm for matrix multiplication[3]. Of course given my background, inspiration to PyTorch[4] should be obvious. Eventual aim is to make this language sort a backend for PyTorch.

References:
1. TODO
3. Turing lecture by Backus