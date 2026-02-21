# LIBRIL

This code is forked from the repo here: https://github.com/sampsyo/bril/tree/main/examples. For now we'll invoke the functions here using command line. Later we will consider converting to relative imports to make it work like a library.

Example:

```bash
$ cat libril/test/to_ssa/if.bril | bril2json | python libril/to_ssa.py | bril2txt 
@main(cond: bool) {
.entry:
  a.0: int = const 47;
  br cond .left .right;
.left:
  a.2: int = add a.0 a.0;
  jmp .exit;
.right:
  a.3: int = mul a.0 a.0;
  jmp .exit;
.exit:
  a.1: int = phi a.2 a.3 .left .right;
  print a.1;
  ret;
}
```

It has lot of useful passes just like this.

Original code is distributed under MIT license. Because MIT is compatible with GPL, I can therefore modify it and redistribute them under GPL license.


-----

Bril Examples
=============

This directory contains assorted examples showing how to do interesting analyses and transformations on Bril programs. They are intentionally under-documented because you should be figuring this stuff out yourself, not reading my half-baked code!
