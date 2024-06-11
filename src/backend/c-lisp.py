#!/usr/bin/env python3
import json
import sys
from utils.random_names import random_label


class CodegenError(Exception):
    pass


class BrilispCodeGenerator:
    def __init__(self):
        self.symbol_types = {}

    def c_lisp(self, prog):
        """Entry point to C-Lisp compiler"""
        if not prog[0] == "c-lisp":
            raise CodegenError("Input not a C-Lisp program")

        return ["brilisp"] + [self.gen_function(fn) for fn in prog[1:]]

    def gen_type(self, typ):
        return typ

    def gen_function(self, func):
        if not func[0] == "define":
            raise CodegenError(f"Not a function: {func}")

        return [
            "define",
            func[1],
            *self.gen_stmt(func[2:]),
        ]

    def gen_stmt(self, stmt):
        try:
            if isinstance(stmt, list):
                if not stmt:
                    return []  #  Null statement
                elif self.is_compound_stmt(stmt):
                    return self.gen_compound_stmt(stmt)
                elif self.is_ret_stmt(stmt):
                    return self.gen_ret_stmt(stmt)
                elif self.is_decl_stmt(stmt):
                    return self.gen_decl_stmt(stmt)
                elif self.is_if_stmt(stmt):
                    return self.gen_if_stmt(stmt)
                else:
                    return self.gen_expr(stmt)
            else:
                return self.gen_expr(stmt)
        except Exception as e:
            print(f"Error in statement: {stmt}")
            raise e

    def is_if_stmt(self, stmt):
        return stmt[0] == "if"

    def gen_if_stmt(self, stmt):
        rand_suffix = random_label()
        cond_sym = "tmp_clisp_" + rand_suffix
        true_lbl = "lbl_true_clisp_" + rand_suffix
        false_lbl = "lbl_false_clisp_" + rand_suffix
        out_lbl = "lbl_out_clisp_" + rand_suffix

        cond_instr_list = self.gen_expr(stmt[1], res_sym=cond_sym)
        true_instr_list = self.gen_stmt(stmt[2])
        if len(stmt) == 3:
            false_instr_list = []
        elif len(stmt) == 4:
            false_instr_list = self.gen_stmt(stmt[3])
        else:
            raise CodegenError(f"Bad if statement: {stmt}")

        return [
            *cond_instr_list,
            ["br", cond_sym, true_lbl, false_lbl],
            ["label", true_lbl],
            *true_instr_list,
            ["jmp", out_lbl],
            ["label", false_lbl],
            *false_instr_list,
            ["label", out_lbl],
        ]

    def is_decl_stmt(self, stmt):
        return stmt[0] == "declare"

    def gen_decl_stmt(self, stmt):
        if not (len(stmt) == 2 and len(stmt[1]) == 2):
            raise CodegenError(f"bad declare statement: {stmt}")

        self.symbol_types[stmt[1][0]] = stmt[1][1]
        return []

    def is_ret_stmt(self, stmt):
        return stmt[0] == "ret"

    def gen_ret_stmt(self, stmt):
        if len(stmt) == 1:
            return ["ret"]
        elif len(stmt) == 2:
            res_sym = random_label("tmp_clisp")
            instr_list = self.gen_expr(stmt[1], res_sym=res_sym)
            instr_list.append(["ret", res_sym])
            return instr_list
        else:
            raise CodegenError(
                f"Return statement can contain only 1 optional return value: {stmt}"
            )

    def is_compound_stmt(self, stmt):
        return isinstance(stmt, list) and isinstance(stmt[0], list)

    def gen_compound_stmt(self, stmt):
        instr_list = []
        for s in stmt:
            instr_list += self.gen_stmt(s)
        return instr_list

    def is_set_expr(self, expr):
        return expr[0] == "set"

    def gen_set_expr(self, expr, res_sym):
        name = expr[1]
        if not name in self.symbol_types:
            raise CodegenError(f"Cannot set undeclared variable: {name}")

        instr_list = self.gen_expr(expr[2], res_sym=res_sym)
        instr_list.append(["set", [name, self.symbol_types[name]], ["id", res_sym]])
        return instr_list

    def get_literal_type(self, expr):
        if isinstance(expr, bool):
            return "bool"
        elif isinstance(expr, int):
            return "int"
        else:
            return None

    def is_literal_expr(self, expr):
        return not (self.get_literal_type(expr) is None)

    def gen_literal_expr(self, expr, res_sym):
        return [["set", [res_sym, self.get_literal_type(expr)], ["const", expr]]]

    def is_call_expr(self, expr):
        return expr[0] == "call"

    def gen_call_expr(self, expr, res_sym):
        instr_list = []
        arg_syms = []
        for arg in expr[2:]:
            arg_sym = random_label("tmp_clisp")
            arg_syms.append(arg_sym)
            instr_list += self.gen_expr(arg, res_sym=arg_sym)
        instr_list.append(["set", [res_sym, "int"], ["call", expr[1], *arg_syms]])
        return instr_list

    def is_var_expr(self, expr):
        return isinstance(expr, str)

    def gen_var_expr(self, expr, res_sym):
        if expr in self.symbol_types:
            return [["set", [res_sym, self.symbol_types[expr]], ["id", expr]]]
        else:
            raise CodegenError(f"Reference to undeclared variable: {expr}")

    def gen_expr(self, expr, res_sym=random_label("tmp_clisp")):
        if self.is_literal_expr(expr):
            return self.gen_literal_expr(expr, res_sym)
        elif self.is_set_expr(expr):
            return self.gen_set_expr(expr, res_sym)
        elif self.is_call_expr(expr):
            return self.gen_call_expr(expr, res_sym)
        elif self.is_var_expr(expr):
            return self.gen_var_expr(expr, res_sym)
        else:
            raise CodegenError(f"Bad expression: {expr}")


if __name__ == "__main__":
    brilisp_code_generator = BrilispCodeGenerator()
    json.dump(brilisp_code_generator.c_lisp(json.loads(sys.stdin.read())), sys.stdout)
