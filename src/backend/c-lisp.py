#!/usr/bin/env python3
import json
import sys
from utils.random import random_label
from utils.shape import verify_shape

CLISP_PREFIX = "tmp_clisp"


class CodegenError(Exception):
    pass


class BrilispCodeGenerator:
    def __init__(self):
        # Type tracking
        self.symbol_types = {}  # Variable name -> type
        self.scopes = []  # Stack of scope tags
        self.function_types = {}  # Function name > (ret-type, (arg-types...))
        self.pointer_types = {}  # For internal use, e.g. temporary pointer variables

        self.fixed_op_types = {
            # <opcode>: <result type>
            # Integer arithmetic
            "add": ("int", 2),
            "sub": ("int", 2),
            "mul": ("int", 2),
            "div": ("int", 2),
            # Integer comparison
            "eq": ("bool", 2),
            "ne": ("bool", 2),
            "lt": ("bool", 2),
            "gt": ("bool", 2),
            "le": ("bool", 2),
            "ge": ("bool", 2),
            # Floating-point arithmetic
            "fadd": ("float", 2),
            "fsub": ("float", 2),
            "fmul": ("float", 2),
            "fdiv": ("float", 2),
            # Floating-point comparison
            "feq": ("bool", 2),
            "fne": ("bool", 2),
            "flt": ("bool", 2),
            "fgt": ("bool", 2),
            "fle": ("bool", 2),
            "fge": ("bool", 2),
            # Boolean logic
            "and": ("bool", 2),
            "or": ("bool", 2),
            "not": ("bool", 1),
        }

    def c_lisp(self, prog):
        """Entry point to C-Lisp compiler"""
        if not prog[0] == "c-lisp":
            raise CodegenError("Input not a C-Lisp program")

        return ["brilisp"] + [self.gen_function(fn) for fn in prog[1:]]

    def construct_scoped_name(self, name, scopes):
        return ".".join([name] + scopes)

    def scoped_lookup(self, name):
        """Look up the name in reverse order of current scope stack"""
        # be as specific as possible first
        for s in range(len(self.scopes), -1, -1):
            scoped_name = self.construct_scoped_name(name, self.scopes[:s])
            if scoped_name in self.symbol_types:
                return scoped_name
        raise CodegenError(f"Undeclared symbol: {name}")

    def gen_function(self, func):
        if not func[0] == "define":
            raise CodegenError(f"Not a function: {func}")

        for elem in func[1]:
            if not len(elem) == 2:
                raise CodegenError(f"Bad function prototype: {func[1]}")

        # Clear the symbol table and scope stack
        self.symbol_types = {}
        self.scopes = []

        name, ret_type = func[1][0]
        parm_types = []
        for parm in func[1][1:]:
            parm_types.append(parm[1])
            self.symbol_types[parm[0]] = parm[1]
        self.function_types[name] = [ret_type, parm_types]

        # If the function has a body, we need to codegen for it
        if len(func) > 2:
            # Set up a return label and a return variable, if needed
            self.ret_ptr_sym, ret_label, ret_alloc_size_sym, ret_val_sym = [
                random_label(CLISP_PREFIX, [extra])
                for extra in ("ret_ptr", "ret_lbl", "ret_alloc", "ret_val")
            ]
            self.ret_jmp_instr = ["jmp", ret_label]
            if ret_type != "void":
                # Function returns something, so we have to maintain a return variable
                ret_alloc_instrs = [
                    # Allocate space for the return variable
                    ["set", [ret_alloc_size_sym, "int"], ["const", 1]],
                    [
                        "set",
                        [self.ret_ptr_sym, ["ptr", ret_type]],
                        ["alloc", ret_alloc_size_sym],
                    ],
                ]
                ret_label_instrs = [
                    # Load the return variable and return it.
                    # Control jumps here for function return.
                    ["label", ret_label],
                    ["set", [ret_val_sym, ret_type], ["load", self.ret_ptr_sym]],
                    ["ret", ret_val_sym],
                ]
            else:
                # No return value, so no need to maintain a return variable
                ret_alloc_instrs = []
                ret_label_instrs = [["label", ret_label], ["ret"]]

            body_instrs = [
                *ret_alloc_instrs,
                *self.gen_compound_stmt(
                    func[2:], new_scope=False
                ),  # C-Lisp function body
                *ret_label_instrs,
            ]
        else:
            # This is a declaration without a body
            body_instrs = []

        return [
            "define",
            func[1],
            *body_instrs,
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
                    return self.gen_expr(stmt)[0]
            else:
                return self.gen_expr(stmt)[0]
        except Exception as e:
            print(f"Error in statement: {stmt}", file=sys.stderr)
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
        cond_expr_instr, cond_sym = self.gen_expr(stmt[1])
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

        loop_lbl, cont_lbl, break_lbl = [
            random_label(CLISP_PREFIX, [extra])
            for extra in (
                "loop",
                "cont",
                "break",
            )
        ]
        init_expr_instr, _ = self.gen_expr(stmt[1][0])
        cond_expr_instr, cond_sym = self.gen_expr(stmt[1][1])
        iter_expr_instr, _ = self.gen_expr(stmt[1][2])
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
        true_lbl, false_lbl, out_lbl = [
            random_label(CLISP_PREFIX, [extra])
            for extra in (
                "true",
                "false",
                "out",
            )
        ]

        cond_instr_list, cond_sym = self.gen_expr(stmt[1])
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
        if not len(stmt) == 3:
            raise CodegenError(f"Bad declare statement: {stmt}")

        name, typ = stmt[1], stmt[2]
        scoped_name = self.construct_scoped_name(name, self.scopes)
        if scoped_name in self.symbol_types:
            raise CodegenError(f"Re-declaration of variable {name}")
        self.symbol_types[scoped_name] = typ
        return []

    def is_ret_stmt(self, stmt):
        return stmt[0] == "ret"

    def gen_ret_stmt(self, stmt):
        if len(stmt) == 1:
            return [self.ret_jmp_instr]

        elif len(stmt) == 2:
            res_instrs, res_sym = self.gen_expr(stmt[1])
            instr_list = [
                *res_instrs,
                ["store", self.ret_ptr_sym, res_sym],
                self.ret_jmp_instr,
            ]
            return instr_list
        else:
            raise CodegenError(
                f"Return statement can contain only 1 optional return value: {stmt}"
            )

    def is_compound_stmt(self, stmt):
        return isinstance(stmt, list) and isinstance(stmt[0], list)

    def gen_compound_stmt(self, stmt, new_scope=True):
        if new_scope:
            scope = random_label()
            self.scopes.append(scope)
        instr_list = []
        for s in stmt:
            instr_list += self.gen_stmt(s)
        if new_scope:
            self.scopes.pop()
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

    def gen_expr(self, expr):
        # literal
        def is_literal_expr(expr):
            return not (self.get_literal_type(expr) is None)

        def gen_literal_expr(expr):
            res_sym = random_label(CLISP_PREFIX)
            return [["set", [res_sym, self.get_literal_type(expr)], ["const", expr]]], res_sym

        # set
        def is_set_expr(expr):
            return expr[0] == "set"

        def gen_set_expr(expr):
            if not verify_shape(expr, [str, str, None]):
                raise CodegenError(f"Bad set expression: {expr}")

            name = expr[1]
            scoped_name = self.scoped_lookup(name)
            instr_list, res_sym = self.gen_expr(expr[2])
            instr_list.append(
                ["set", [scoped_name, self.symbol_types[scoped_name]], ["id", res_sym]]
            )
            return instr_list, scoped_name

        # call
        def is_call_expr(expr):
            return expr[0] == "call"

        def gen_call_expr(expr):
            instr_list = []
            arg_syms = []
            for arg in expr[2:]:
                arg_instr_list, arg_sym = self.gen_expr(arg)
                arg_syms.append(arg_sym)
                instr_list.extend(arg_instr_list)

            name = expr[1]
            if name not in self.function_types:
                raise CodegenError(f"Call to undeclared function: {name}")
            
            res_sym = random_label(CLISP_PREFIX)
            instr_list.append(
                [
                    "set",
                    [res_sym, self.function_types[name][0]],
                    ["call", name, *arg_syms],
                ]
            )
            return instr_list, res_sym

        # var
        def is_var_expr(expr):
            return isinstance(expr, str)

        def gen_var_expr(expr):
            scoped_name = self.scoped_lookup(expr)
            typ = self.symbol_types[scoped_name]
            if typ[0] == "ptr":
                self.pointer_types[scoped_name] = typ
            return [], scoped_name

        # fixed type
        def is_fixed_type_expr(expr):
            return expr[0] in self.fixed_op_types

        def gen_fixed_type_expr(expr):
            instr_list = []
            opcode = expr[0]
            typ, n_ops = self.fixed_op_types[opcode]

            if not (len(expr) == n_ops + 1):
                raise CodegenError(f"`{opcode}` takes only {n_ops} operands: {expr}")
            
            in_syms = []
            input_instrs = []
            for n in range(n_ops):
                inp_instrs, inp_sym = self.gen_expr(expr[n + 1])
                input_instrs.extend(inp_instrs)
                in_syms.append(inp_sym)

            res_sym = random_label(CLISP_PREFIX)
            return [
                *input_instrs,
                ["set", [res_sym, typ], [opcode, *in_syms]],
            ], res_sym

        # ptradd
        def is_ptradd_expr(expr):
            return expr[0] == "ptradd"

        def gen_ptradd_expr(expr):
            if len(expr) != 3:
                raise CodegenError(f"Bad ptradd expression: {expr}")

            offset_instrs, offset_sym = self.gen_expr(expr[2])
            
            ptr_name = self.scoped_lookup(expr[1])
            ptr_type = self.symbol_types[ptr_name]
            res_sym = random_label(CLISP_PREFIX)

            self.pointer_types[res_sym] = ptr_type
            return [
                *offset_instrs,
                ["set", [res_sym, ptr_type], ["ptradd", ptr_name, offset_sym]],
            ], res_sym

        # load
        def is_load_expr(expr):
            return expr[0] == "load"

        def gen_load_expr(expr):
            if len(expr) != 2:
                raise CodegenError(f"Bad load expression: {expr}")

            ptr_instrs, ptr_sym = self.gen_expr(expr[1])
            res_sym = random_label(CLISP_PREFIX)
            return [
                *ptr_instrs,
                ["set", [res_sym, self.pointer_types[ptr_sym][1]], ["load", ptr_sym]],
            ], res_sym

        # store
        def is_store_expr(expr):
            return expr[0] == "store"

        def gen_store_expr(expr):
            if len(expr) != 3:
                raise CodegenError(f"Bad store expression: {expr}")

            ptr_instrs, ptr_sym = self.gen_expr(expr[1])
            val_instrs, val_sym = self.gen_expr(expr[2])

            return [
                *ptr_instrs,
                *val_instrs,
                ["store", ptr_sym, val_sym],
            ], val_sym

        # alloc
        def is_alloc_expr(expr):
            return expr[0] == "alloc"

        def gen_alloc_expr(expr):
            if len(expr) != 3:
                raise CodegenError(f"Bad alloc expression: {expr}")

            ptr_type = ["ptr", expr[1]]
            res_sym = random_label(CLISP_PREFIX)
            self.pointer_types[res_sym] = ptr_type
            size_instrs, size_sym = self.gen_expr(expr[2])
            return [
                *size_instrs,
                ["set", [res_sym, ptr_type], ["alloc", size_sym]],
            ], res_sym

        if is_literal_expr(expr):
            return gen_literal_expr(expr)
        elif is_set_expr(expr):
            return gen_set_expr(expr)
        elif is_call_expr(expr):
            return gen_call_expr(expr)
        elif is_var_expr(expr):
            return gen_var_expr(expr)
        elif is_fixed_type_expr(expr):
            return gen_fixed_type_expr(expr)
        elif is_ptradd_expr(expr):
            return gen_ptradd_expr(expr)
        elif is_load_expr(expr):
            return gen_load_expr(expr)
        elif is_store_expr(expr):
            return gen_store_expr(expr)
        elif is_alloc_expr(expr):
            return gen_alloc_expr(expr)
        else:
            raise CodegenError(f"Bad expression: {expr}")


if __name__ == "__main__":
    brilisp_code_generator = BrilispCodeGenerator()
    json.dump(brilisp_code_generator.c_lisp(json.loads(sys.stdin.read())), sys.stdout)
