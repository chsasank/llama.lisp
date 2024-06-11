#!/usr/bin/env python3
import json
import sys
import random
import string


class CodegenError(Exception):
    pass


def c_lisp(prog):
    """Entry point to C-Lisp compiler"""
    if not prog[0] == "c-lisp":
        raise CodegenError("Input not a C-Lisp program")

    return ["brilisp"] + [gen_function(fn) for fn in prog[1:]]


def get_tmp_sym():
    return "tmp_clisp_" + "".join(
        [random.choice(string.ascii_letters) for i in range(6)]
    )


def gen_type(typ):
    return typ


def gen_function(func):
    if not func[0] == "define":
        raise CodegenError(f"Not a function: {func}")

    return [
        "define",
        func[1],
        *gen_stmt(func[2:]),
    ]


def gen_stmt(stmt):
    if not stmt:
        return [] #  Null statement
    elif is_compound_stmt(stmt):
        return gen_compound_stmt(stmt)
    elif is_ret_stmt(stmt):
        return gen_ret_stmt(stmt)
    else:
        return gen_expr(stmt)


def is_ret_stmt(stmt):
    return stmt[0] == "ret"

def gen_ret_stmt(stmt):
    if len(stmt) == 1:
        return ["ret"]
    elif len(stmt) == 2:
        res_sym = get_tmp_sym()
        instr_list = gen_expr(stmt[1], res_sym = res_sym)
        instr_list.append(["ret", res_sym])
        return instr_list
    else:
        raise CodegenError(f"Return statement can contain only 1 optional return value: {stmt}")

def is_compound_stmt(stmt):
    return isinstance(stmt, list) and isinstance(stmt[0], list)

def gen_compound_stmt(stmt):
    instr_list = []
    for s in stmt:
        instr_list += gen_stmt(s)
    return instr_list

def is_set_expr(expr):
    return expr[0] == "set"


def gen_set_expr(expr, res_sym):
    instr_list = gen_expr(expr[2], res_sym=res_sym)
    instr_list.append(["set", expr[1], ["id", res_sym]])
    return instr_list

def get_literal_type(expr):
    if isinstance(expr, int):
        return "int"
    else:
        return False

def gen_literal_expr(expr, res_sym):
    return [["set", [res_sym, get_literal_type(expr)], ["const", expr]]]

def is_call_expr(expr):
    return expr[0] == "call"

def gen_call_expr(expr, res_sym):
    instr_list = []
    arg_syms = []
    for arg in expr[2:]:
        arg_sym = get_tmp_sym()
        arg_syms.append(arg_sym)
        instr_list += gen_expr(arg, res_sym=arg_sym)
    instr_list.append(["set", [res_sym, "int"], ["call", expr[1], *arg_syms]])
    return instr_list

def gen_expr(expr, res_sym=get_tmp_sym()):
    if get_literal_type(expr):
        return gen_literal_expr(expr, res_sym)
    elif is_set_expr(expr):
        return gen_set_expr(expr, res_sym)
    elif is_call_expr(expr):
        return gen_call_expr(expr, res_sym)
    else:
        raise CodegenError(f"Expected an expression: {expr}")


if __name__ == "__main__":
    json.dump(c_lisp(json.loads(sys.stdin.read())), sys.stdout)
