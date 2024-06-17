#!/usr/bin/env python3
import json
import sys
from utils.random import random_label

CLISP_PREFIX = "tmp_clisp"


class CodegenError(Exception):
    pass


class BrilispCodeGenerator:
    def __init__(self):
        # Type tracking
        self.symbol_types = {}  # Variable name -> type
        self.function_types = {}  # Function name > (ret-type, (arg-types...))

        self.binary_op_types = {
            # <opcode>: <result type>
            # Integer arithmetic
            "add": "int",
            "sub": "int",
            "mul": "int",
            "div": "int",
            # Integer comparison
            "eq": "bool",
            "ne": "bool",
            "lt": "bool",
            "gt": "bool",
            "le": "bool",
            "ge": "bool",
            # Floating-point arithmetic
            "fadd": "float",
            "fsub": "float",
            "fmul": "float",
            "fdiv": "float",
            # Floating-point comparison
            "feq": "bool",
            "fne": "bool",
            "flt": "bool",
            "fgt": "bool",
            "fle": "bool",
            "fge": "bool",
        }

    def c_lisp(self, prog):
        """Entry point to C-Lisp compiler"""
        if not prog[0] == "c-lisp":
            raise CodegenError("Input not a C-Lisp program")

        return ["brilisp"] + [self.gen_function(fn) for fn in prog[1:]]

    def gen_function(self, func):
        if not func[0] == "define":
            raise CodegenError(f"Not a function: {func}")

        for elem in func[1]:
            if not len(elem) == 2:
                raise CodegenError(f"Bad function prototype: {func[1]}")

        self.symbol_types = {}  # Clear the symbol table
        name, ret_type = func[1][0]
        parm_types = []
        for parm in func[1][1:]:
            parm_types.append(parm[1])
            self.symbol_types[parm[0]] = parm[1]
        self.function_types[name] = [ret_type, parm_types]
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
                elif self.is_for_stmt(stmt):
                    return self.gen_for_stmt(stmt)
                elif self.is_while_stmt(stmt):
                    return self.gen_while_stmt(stmt)
                else:
                    return self.gen_expr(stmt)
            else:
                return self.gen_expr(stmt)
        except Exception as e:
            print(f"Error in statement: {stmt}")
            raise e

    def is_while_stmt(self, stmt):
        return stmt[0] == "while"

    def gen_while_stmt(self, stmt):
        if len(stmt) < 3:
            raise CodegenError(f"Bad while statement: {stmt}")

        cond_sym, loop_lbl, cont_lbl, break_lbl = [
            random_label(CLISP_PREFIX, [extra])
            for extra in (
                "cond",
                "loop",
                "cont",
                "break",
            )
        ]
        cond_expr_instr = self.gen_expr(stmt[1], res_sym=cond_sym)
        loop_stmt_instr = self.gen_stmt(stmt[2:])

        return [
            ["label", loop_lbl],
            *cond_expr_instr,
            ["br", cond_sym, cont_lbl, break_lbl],
            ["label", cont_lbl],
            *loop_stmt_instr,
            ["jmp", loop_lbl],
            ["label", break_lbl],
        ]

    def is_for_stmt(self, stmt):
        return stmt[0] == "for"

    def gen_for_stmt(self, stmt):
        if len(stmt) < 3:
            raise CodegenError(f"Bad for statement: {stmt}")

        cond_sym, loop_lbl, cont_lbl, break_lbl = [
            random_label(CLISP_PREFIX, [extra])
            for extra in (
                "cond",
                "loop",
                "cont",
                "break",
            )
        ]
        init_expr_instr = self.gen_expr(stmt[1][0])
        cond_expr_instr = self.gen_expr(stmt[1][1], res_sym=cond_sym)
        iter_expr_instr = self.gen_expr(stmt[1][2])
        loop_stmt_instr = self.gen_stmt(stmt[2:])

        return [
            *init_expr_instr,
            ["label", loop_lbl],
            *cond_expr_instr,
            ["br", cond_sym, cont_lbl, break_lbl],
            ["label", cont_lbl],
            *loop_stmt_instr,
            *iter_expr_instr,
            ["jmp", loop_lbl],
            ["label", break_lbl],
        ]

    def is_if_stmt(self, stmt):
        return stmt[0] == "if"

    def gen_if_stmt(self, stmt):
        cond_sym, true_lbl, false_lbl, out_lbl = [
            random_label(CLISP_PREFIX, [extra])
            for extra in (
                "cond",
                "lbl_true",
                "lbl_false",
                "lbl_out",
            )
        ]

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

        name, typ = stmt[1]
        if name in self.symbol_types:
            raise CodegenError(f"Re-declaration of variable {name}")
        self.symbol_types[name] = typ
        return []

    def is_ret_stmt(self, stmt):
        return stmt[0] == "ret"

    def gen_ret_stmt(self, stmt):
        if len(stmt) == 1:
            return [["ret"]]
        elif len(stmt) == 2:
            res_sym = random_label(CLISP_PREFIX)
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
        elif isinstance(expr, float):
            return "float"
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
            arg_sym = random_label(CLISP_PREFIX)
            arg_syms.append(arg_sym)
            instr_list += self.gen_expr(arg, res_sym=arg_sym)
        name = expr[1]
        instr_list.append(
            ["set", [res_sym, self.function_types[name][0]], ["call", name, *arg_syms]]
        )
        return instr_list

    def is_var_expr(self, expr):
        return isinstance(expr, str)

    def gen_var_expr(self, expr, res_sym):
        if expr in self.symbol_types:
            return [["set", [res_sym, self.symbol_types[expr]], ["id", expr]]]
        else:
            raise CodegenError(f"Reference to undeclared variable: {expr}")

    def is_binary_expr(self, expr):
        return expr[0] in self.binary_op_types

    def gen_binary_expr(self, expr, res_sym):
        if not len(expr) == 3:
            raise CodegenError(f"Binary operation takes only 2 operands: {expr}")

        instr_list = []
        in1_sym, in2_sym = [
            random_label(CLISP_PREFIX, [extra]) for extra in ("in1", "in2")
        ]
        opcode = expr[0]
        typ = self.binary_op_types[opcode]
        return [
            *self.gen_expr(expr[1], in1_sym),
            *self.gen_expr(expr[2], in2_sym),
            ["set", [res_sym, typ], [opcode, in1_sym, in2_sym]],
        ]

    def gen_expr(self, expr, res_sym=None):
        res_sym = res_sym or random_label(CLISP_PREFIX)
        if self.is_literal_expr(expr):
            return self.gen_literal_expr(expr, res_sym)
        elif self.is_set_expr(expr):
            return self.gen_set_expr(expr, res_sym)
        elif self.is_call_expr(expr):
            return self.gen_call_expr(expr, res_sym)
        elif self.is_var_expr(expr):
            return self.gen_var_expr(expr, res_sym)
        elif self.is_binary_expr(expr):
            return self.gen_binary_expr(expr, res_sym)
        else:
            raise CodegenError(f"Bad expression: {expr}")


if __name__ == "__main__":
    brilisp_code_generator = BrilispCodeGenerator()
    json.dump(brilisp_code_generator.c_lisp(json.loads(sys.stdin.read())), sys.stdout)
