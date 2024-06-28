#!/usr/bin/env python3

import re
from sys import stdin

class List(list):
    def __str__(self):
        return "(" + " ".join([str(e) for e in self]) + ")"

class Bool:
    def __init__(self, val):
        if val == "#t":
            bool.__init__(self, True)
        elif val == "#f":
            bool.__init__(self, False)
    def __str__(self):
        if self:
            return "#t"
        else:
            return "#f"

class Symbol(str):
    def __init__(self, val):
        self.symbol = val[1:]

    def __str__(self):
        return self.symbol

class Var(str):
    def __str__(self):
        return self

class Expr(list):
    pass

symtab = {}

def eval_expr(expr):
    if isinstance(expr, Expr):
        return library[expr[0]](expr)
    elif isinstance(expr, Var):
        return symtab[expr]
    else:
        return expr

def eval_macro(expr):
    name, args = expr[0], expr[1:]
    macro_header, macro_body = macros[name]
    macro_name, macro_args = macro_header[0], macro_header[1:]
    for i in range(len(macro_args)):
        symtab[macro_args[i]] = eval_quasiquote(args[i])
    return eval_expr(macro_body)

def define_macro(expr):
    header, body = expr[1], expr[2]
    macros[header[0]] = (header, Expr(body))
    return ""

def eval_quasiquote(expr):
    if isinstance(expr, Expr):
        if isinstance(expr[0], str) and (expr[0] in macros):
            return eval_macro(expr)
        elif expr[0] == "unquote":
            return eval_expr(expr)
        else:
            return List(eval_quasiquote(subexp) for subexp in expr)
    else:
        return expr

library = {
    "apply": lambda expr: eval_expr([expr[0], *expr[1:]]),
    "list": lambda expr: List([eval_expr(e) for e in expr[1:]]),
    "c-lisp": eval_quasiquote,
    "define-macro": define_macro,
    "eval-macro": eval_macro,
    "quasiquote": eval_quasiquote,
    "unquote": eval_expr,
}

macros = {}


def Token(tok_str):
    TOKEN_TYPES = (
        (r"^[0-9]*\.[0-9]+$", float),
        (r"^[0-9]+$", int),
        (r"'", Symbol),
        (r"#", Bool),
        (r"", Var),
    )
    for pat, handler in TOKEN_TYPES:
        if re.match(pat, tok_str):
            return handler(tok_str)

def parse (program_string):
    token_list = program_string.replace ("(", " ( ").replace (")", " ) ").split()
    program = Expr()
    list_stack = [program]

    for tok in token_list:
        if tok == "(":
            list_stack.append(Expr())
        elif tok == ")":
            list_top = list_stack.pop()
            list_stack[-1].append(list_top)
        else:
            list_stack[-1].append(Token(tok))

    return program

def repl ():
    paren_depth = 0
    tmp_expr = []

    print("prelisp> ", end="", flush=True)

    while True:
        ch = stdin.read(1)
        tmp_expr.append(ch)
        if ch == "(":
            paren_depth += 1
        elif ch == ")":
            paren_depth -= 1
        elif ch == "\n":
            if paren_depth == 0:
                expr = parse("".join(tmp_expr))[0]
                print (eval_expr(expr), flush=True)
                print ("prelisp> ", end="", flush=True)
                tmp_expr = []


if __name__ == "__main__":
    if stdin.isatty():
        repl()
    else:
        program = parse(stdin.read())
        print (eval_expr(program[0]))
        for expr in program:
            print(eval_expr(expr))
