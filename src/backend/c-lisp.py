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
        ## Type tracking
        # Variable name -> type
        self.symbol_types = {}
        # Stack of scope tags
        self.scopes = []
        # Function name > (ret-type, (arg-types...))
        self.function_types = {}

        int_types = {"int8", "int16", "int", "int32", "int64"}
        float_types = {"float", "double"}
        self.arith_op_types = {
            ## opcode -> possible operand and result types
            # Integer arithmetic
            "add": int_types,
            "sub": int_types,
            "mul": int_types,
            "div": int_types,
            # Floating-point arithmetic
            "fadd": float_types,
            "fsub": float_types,
            "fmul": float_types,
            "fdiv": float_types,
        }
        self.comp_op_types = {
            ## opcode -> possible operand and result types
            # Integer comparison
            "eq": int_types,
            "ne": int_types,
            "lt": int_types,
            "gt": int_types,
            "le": int_types,
            "ge": int_types,
            # Floating-point comparison
            "feq": float_types,
            "fne": float_types,
            "flt": float_types,
            "fgt": float_types,
            "fle": float_types,
            "fge": float_types,
        }
        self.logic_op_types = {
            ## opcode -> number of operands
            # Boolean logic
            "and",
            "or",
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
        cond_expr_instr, cond_sym, _ = self.gen_expr(stmt[1])
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
        init_expr_instr, _, _ = self.gen_expr(stmt[1][0])
        cond_expr_instr, cond_sym, _ = self.gen_expr(stmt[1][1])
        iter_expr_instr, _, _ = self.gen_expr(stmt[1][2])
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

        cond_instr_list, cond_sym, _ = self.gen_expr(stmt[1])
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
            res_instrs, res_sym, _ = self.gen_expr(stmt[1])
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
            res_type = self.get_literal_type(expr)
            return [["set", [res_sym, res_type], ["const", expr]]], res_sym, res_type

        # set
        def is_set_expr(expr):
            return expr[0] == "set"

        def gen_set_expr(expr):
            if not verify_shape(expr, [str, str, None]):
                raise CodegenError(f"Bad set expression: {expr}")

            name = expr[1]
            scoped_name = self.scoped_lookup(name)
            instr_list, res_sym, res_type = self.gen_expr(expr[2])
            instr_list.append(["set", [scoped_name, res_type], ["id", res_sym]])
            return instr_list, scoped_name, res_type

        # call
        def is_call_expr(expr):
            return expr[0] == "call"

        def gen_call_expr(expr):
            instr_list = []
            arg_syms = []
            for arg in expr[2:]:
                arg_instr_list, arg_sym, arg_typ = self.gen_expr(arg)
                arg_syms.append(arg_sym)
                instr_list.extend(arg_instr_list)

            name = expr[1]
            if name not in self.function_types:
                raise CodegenError(f"Call to undeclared function: {name}")

            res_sym = random_label(CLISP_PREFIX)
            ret_type = self.function_types[name][0]
            instr_list.append(
                [
                    "set",
                    [res_sym, ret_type],
                    ["call", name, *arg_syms],
                ]
            )
            return instr_list, res_sym, ret_type

        # var
        def is_var_expr(expr):
            return isinstance(expr, str)

        def gen_var_expr(expr):
            scoped_name = self.scoped_lookup(expr)
            return [], scoped_name, self.symbol_types[scoped_name]

        def gen_compute_expr_wrapper(*, allowed_types_getter, result_type_getter):
            """
            Closure to generalize codegen for computation expressions. Assumes that the
            opcode takes 2 operands of the same type.
            allowed_types_getter(opcode) should return a set of allowed operand types
            result_type_getter(operand_types) should return the result type
            """
            def gen_compute_expr(expr):
                opcode = expr[0]
                if not verify_shape(expr, [str, None, None]):
                    raise CodegenError(f"opcode {opcode} takes exactly 2 operands")

                input_instr_list, operand_syms, operand_types = [], [], []
                allowed_types = allowed_types_getter(opcode)
                for ex in expr[1:]:
                    instrs, sym, typ = self.gen_expr(ex)
                    if not (isinstance(typ, str) and typ in allowed_types):
                        raise CodegenError(
                            f"Operands to {opcode} must be one of {allowed_types}"
                        )
                    input_instr_list += instrs
                    operand_types.append(typ)
                    operand_syms.append(sym)
                if operand_types[0] != operand_types[1]:
                    raise CodegenError(f"Operands to `{opcode}` must have same width")

                res_sym = random_label(CLISP_PREFIX)
                res_type = result_type_getter(operand_types)
                return (
                    [
                        *input_instr_list,
                        ["set", [res_sym, res_type], [opcode, *operand_syms]],
                    ],
                    res_sym,
                    res_type,
                )
            return gen_compute_expr

        # arithmetic
        def is_arith_expr(expr):
            return expr[0] in self.arith_op_types

        gen_arith_expr = gen_compute_expr_wrapper(
            allowed_types_getter = lambda opcode: self.arith_op_types[opcode],
            result_type_getter = lambda operand_types: operand_types[0],
        )

        # comparison
        def is_comp_expr(expr):
            return expr[0] in self.comp_op_types

        gen_comp_expr = gen_compute_expr_wrapper(
            allowed_types_getter = lambda opcode: self.comp_op_types[opcode],
            result_type_getter = lambda operand_types: "bool",
        )

        # boolean logic
        def is_logic_expr(expr):
            return expr[0] in self.logic_op_types

        gen_logic_expr = gen_compute_expr_wrapper(
            allowed_types_getter = lambda opcode: "bool",
            result_type_getter = lambda operand_types: "bool",
        )

        # boolean not. Cannot be expressed using gen_compute_expr_wrapper
        def is_not_expr(expr):
            return expr[0] == "not"

        def gen_not_expr(expr):
            if len(expr) != 2:
                raise CodegenError(f"not takes exactly 1 operand")
            input_instr_list, input_sym, input_type = self.gen_expr(expr[1])
            if input_type != "bool":
                raise CodegenError(f"Operand to `not` must be bool")
            res_sym = random_label(CLISP_PREFIX)
            return [
                *input_instr_list,
                ["set", [res_sym, "bool"], ["not", input_sym]]
            ], res_sym, "bool"


        # ptradd
        def is_ptradd_expr(expr):
            return expr[0] == "ptradd"

        def gen_ptradd_expr(expr):
            if len(expr) != 3:
                raise CodegenError(f"Bad ptradd expression: {expr}")

            offset_instrs, offset_sym, offset_type = self.gen_expr(expr[2])
            ptr_name = self.scoped_lookup(expr[1])
            ptr_type = self.symbol_types[ptr_name]
            res_sym = random_label(CLISP_PREFIX)

            return (
                [
                    *offset_instrs,
                    ["set", [res_sym, ptr_type], ["ptradd", ptr_name, offset_sym]],
                ],
                res_sym,
                ptr_type,
            )

        # load
        def is_load_expr(expr):
            return expr[0] == "load"

        def gen_load_expr(expr):
            if len(expr) != 2:
                raise CodegenError(f"Bad load expression: {expr}")

            ptr_instrs, ptr_sym, ptr_type = self.gen_expr(expr[1])
            res_sym = random_label(CLISP_PREFIX)
            return (
                [
                    *ptr_instrs,
                    ["set", [res_sym, ptr_type[1]], ["load", ptr_sym]],
                ],
                res_sym,
                ptr_type[1],
            )

        # store
        def is_store_expr(expr):
            return expr[0] == "store"

        def gen_store_expr(expr):
            if len(expr) != 3:
                raise CodegenError(f"Bad store expression: {expr}")

            ptr_instrs, ptr_sym, ptr_type = self.gen_expr(expr[1])
            val_instrs, val_sym, val_type = self.gen_expr(expr[2])
            return (
                [
                    *ptr_instrs,
                    *val_instrs,
                    ["store", ptr_sym, val_sym],
                ],
                val_sym,
                val_type,
            )

        # alloc
        def is_alloc_expr(expr):
            return expr[0] == "alloc"

        def gen_alloc_expr(expr):
            if len(expr) != 3:
                raise CodegenError(f"Bad alloc expression: {expr}")

            ptr_type = ["ptr", expr[1]]
            res_sym = random_label(CLISP_PREFIX)
            size_instrs, size_sym, _ = self.gen_expr(expr[2])
            return (
                [
                    *size_instrs,
                    ["set", [res_sym, ptr_type], ["alloc", size_sym]],
                ],
                res_sym,
                ptr_type,
            )

        # pointer to variable
        def is_ptr_to_expr(expr):
            return expr[0] == "ptr-to"

        def gen_ptr_to_expr(expr):
            if not verify_shape(expr, [str, str]):
                raise CodegenError(f"Bad ptr-to expression: {expr}")

            scoped_name = self.scoped_lookup(expr[1])
            res_type = self.symbol_types[scoped_name]
            res_sym = random_label(CLISP_PREFIX)
            return [
                ["set", [res_sym, ["ptr", res_type]], ["ptr-to", scoped_name]]
            ], res_sym, res_type

        if is_literal_expr(expr):
            return gen_literal_expr(expr)
        elif is_set_expr(expr):
            return gen_set_expr(expr)
        elif is_call_expr(expr):
            return gen_call_expr(expr)
        elif is_var_expr(expr):
            return gen_var_expr(expr)
        elif is_arith_expr(expr):
            return gen_arith_expr(expr)
        elif is_comp_expr(expr):
            return gen_comp_expr(expr)
        elif is_logic_expr(expr):
            return gen_logic_expr(expr)
        elif is_not_expr(expr):
            return gen_not_expr(expr)
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
