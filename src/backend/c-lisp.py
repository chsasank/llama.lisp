#!/usr/bin/env python3
import json
import sys
from utils.random import random_label
from utils.shape import verify_shape

CLISP_PREFIX = "tmp_clisp"


class CodegenError(Exception):
    pass


class ExpressionResult:
    def __init__(self, instructions, symbol, typ):
        self.instructions = instructions
        self.symbol = symbol
        self.typ = typ


## Main Generator
class BrilispCodeGenerator:
    def __init__(self):
        # Stack of scope tags
        self.scopes = []

        ## Type tracking
        # Variable name -> type
        self.variable_types = {}
        # Function name > (ret-type, (arg-types...))
        self.function_types = {}
        # Struct type name -> {field name -> (index number, type)}
        self.struct_types = {}
        # String literals
        self.string_literals = {}

    def c_lisp(self, prog):
        """Entry point to C-Lisp compiler"""
        if not prog[0] == "c-lisp":
            raise CodegenError("Input not a C-Lisp program")

        brilisp_funcs = []
        brilisp_structs = []
        for defn in prog[1:]:
            if defn[0] == "define":
                brilisp_funcs.append(self.gen_function(defn))
            elif defn[0] == "define-struct":
                brilisp_structs.append(self.gen_struct(defn))
            else:
                raise CodegenError(f"Neither function nor struct definition: {defn}")

        brilisp_strings = [
            ["define-string", name, ["string", val]]
            for name, val in self.string_literals.items()
        ]
        return [
            "brilisp",
            *brilisp_strings,
            *brilisp_structs,
            *brilisp_funcs,
        ]

    def construct_scoped_name(self, name, scopes):
        return ".".join([name] + scopes)

    def scoped_lookup(self, name):
        """Look up the name in reverse order of current scope stack"""
        # be as specific as possible first
        for s in range(len(self.scopes), -1, -1):
            scoped_name = self.construct_scoped_name(name, self.scopes[:s])
            if scoped_name in self.variable_types:
                return scoped_name
        raise CodegenError(f"Undeclared symbol: {name}")

    def gen_function(self, func):
        if not func[0] == "define":
            raise CodegenError(f"Not a function: {func}")

        for elem in func[1]:
            if not len(elem) == 2:
                raise CodegenError(f"Bad function prototype: {func[1]}")

        # Clear the symbol table and scope stack
        self.variable_types = {}
        self.scopes = []

        name, ret_type = func[1][0]
        parm_types = []
        for parm in func[1][1:]:
            parm_types.append(parm[1])
            self.variable_types[parm[0]] = parm[1]
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

    def gen_struct(self, struct):
        name = struct[1]
        struct_membs = []
        self.struct_types[name] = {}
        for idx, (field, typ) in enumerate(struct[2:]):
            self.struct_types[name][field] = (idx, typ)
            struct_membs.append(typ)

        return ["define-struct", name, struct_membs]

    def gen_stmt(self, stmt):
        try:
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
                return self.gen_expr(stmt).instructions
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
        cond_expr = self.gen_expr(stmt[1])
        loop_stmt_instr = self.gen_stmt(stmt[2:])

        return [
            ["label", loop_lbl],
            *cond_expr.instructions,
            ["br", cond_expr.symbol, cont_lbl, break_lbl],
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
        init_expr = self.gen_expr(stmt[1][0])
        cond_expr = self.gen_expr(stmt[1][1])
        iter_expr = self.gen_expr(stmt[1][2])
        loop_stmt_instr = self.gen_stmt(stmt[2:])

        return [
            *init_expr.instructions,
            ["label", loop_lbl],
            *cond_expr.instructions,
            ["br", cond_expr.symbol, cont_lbl, break_lbl],
            ["label", cont_lbl],
            *loop_stmt_instr,
            *iter_expr.instructions,
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

        cond_expr = self.gen_expr(stmt[1])
        true_instr_list = self.gen_stmt(stmt[2])
        if len(stmt) == 3:
            false_instr_list = []
        elif len(stmt) == 4:
            false_instr_list = self.gen_stmt(stmt[3])
        else:
            raise CodegenError(f"Bad if statement: {stmt}")

        return [
            *cond_expr.instructions,
            ["br", cond_expr.symbol, true_lbl, false_lbl],
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
        if scoped_name in self.variable_types:
            raise CodegenError(f"Re-declaration of variable {name}")
        self.variable_types[scoped_name] = typ

        instr_list = [["set", [scoped_name, typ], ["const", None]]]
        return instr_list

    def is_ret_stmt(self, stmt):
        return stmt[0] == "ret"

    def gen_ret_stmt(self, stmt):
        if len(stmt) == 1:
            return [self.ret_jmp_instr]

        elif len(stmt) == 2:
            res_expr = self.gen_expr(stmt[1])
            instr_list = [
                *res_expr.instructions,
                ["store", self.ret_ptr_sym, res_expr.symbol],
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

    def gen_expr(self, expr):
        return Expression(self).compile(expr)


## Expressions
class Expression:
    expr_types = []

    def __init__(self, ctx):
        self.ctx = ctx

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        cls.expr_types.append(cls)

    @classmethod
    def is_valid_expr(cls, expr):
        raise NotImplementedError

    def compile(self, expr):
        for expr_type in self.expr_types:
            if expr_type.is_valid_expr(expr):
                result = expr_type(self.ctx).compile(expr)
                assert isinstance(result, ExpressionResult)
                return result

        raise CodegenError(f"Bad expression: {expr}")


class LiteralExpression(Expression):
    @staticmethod
    def get_literal_type(expr):
        if isinstance(expr, bool):
            return "bool"
        elif isinstance(expr, int):
            return "int"
        elif isinstance(expr, float):
            return "float"
        else:
            return None

    @classmethod
    def is_valid_expr(cls, expr):
        return not (cls.get_literal_type(expr) is None)

    def compile(self, expr):
        res_sym = random_label(CLISP_PREFIX)
        res_type = self.get_literal_type(expr)
        instructions = [["set", [res_sym, res_type], ["const", expr]]]
        return ExpressionResult(instructions, res_sym, res_type)


class StringExpression(Expression):
    @classmethod
    def is_valid_expr(cls, expr):
        return expr[0] == "string"

    def compile(self, expr):
        if not verify_shape(expr, ["string", str]):
            raise CodegenError(f"Invalid string literal: {expr}")

        str_sym, res_sym = [random_label(CLISP_PREFIX) for i in range(2)]
        self.ctx.string_literals[str_sym] = expr[1]
        res_typ = ["ptr", "int8"]
        instrs = [["set", [res_sym, res_typ], ["string-ref", str_sym]]]
        return ExpressionResult(instrs, res_sym, res_typ)


class SetExpression(Expression):
    @classmethod
    def is_valid_expr(cls, expr):
        return expr[0] == "set"

    def compile(self, expr):
        if not verify_shape(expr, [str, str, None]):
            raise CodegenError(f"Bad set expression: {expr}")

        name = expr[1]
        scoped_name = self.ctx.scoped_lookup(name)
        res = super().compile(expr[2])
        instructions = res.instructions + [
            [
                "set",
                [scoped_name, res.typ],
                ["id", res.symbol],
            ]
        ]
        return ExpressionResult(instructions, scoped_name, res.typ)


class CallExpression(Expression):
    @classmethod
    def is_valid_expr(cls, expr):
        return expr[0] == "call"

    def compile(self, expr):
        instr_list = []
        arg_syms = []
        arg_types = []
        name = expr[1]
        if name not in self.ctx.function_types:
            raise CodegenError(f"Call to undeclared function: {name}")

        for arg in expr[2:]:
            arg = super().compile(arg)
            arg_types.append(arg.typ)
            arg_syms.append(arg.symbol)
            instr_list.extend(arg.instructions)

        expected_arg_types = self.ctx.function_types[name][1]
        if arg_types != expected_arg_types:
            raise CodegenError(
                f"Expected types: {expected_arg_types}, Received: {arg_types}"
            )

        res_sym = random_label(CLISP_PREFIX)
        ret_type = self.ctx.function_types[name][0]
        instr_list.append(
            [
                "set",
                [res_sym, ret_type],
                ["call", name, *arg_syms],
            ]
        )
        return ExpressionResult(instr_list, res_sym, ret_type)


class VarExpression(Expression):
    @classmethod
    def is_valid_expr(cls, expr):
        return isinstance(expr, str)

    def compile(self, expr):
        instructions = []
        symbol = self.ctx.scoped_lookup(expr)
        typ = self.ctx.variable_types[symbol]
        return ExpressionResult(instructions, symbol, typ)


class BinOpExpression(Expression):
    op_codes = {
        ## opcode -> possible operand and result types
        ## None -> same as input type
        # Integer arithmetic
        "add": ("_int", None),
        "sub": ("_int", None),
        "mul": ("_int", None),
        "div": ("_int", None),
        # Floating-point arithmetic
        "fadd": ("_float", None),
        "fsub": ("_float", None),
        "fmul": ("_float", None),
        "fdiv": ("_float", None),
        # Integer comparison
        "eq": ("_int", "bool"),
        "ne": ("_int", "bool"),
        "lt": ("_int", "bool"),
        "gt": ("_int", "bool"),
        "le": ("_int", "bool"),
        "ge": ("_int", "bool"),
        # Floating-point comparison
        "feq": ("_float", "bool"),
        "fne": ("_float", "bool"),
        "flt": ("_float", "bool"),
        "fgt": ("_float", "bool"),
        "fle": ("_float", "bool"),
        "fge": ("_float", "bool"),
        # Boolean logic
        "and": ("bool", "bool"),
        "or": ("bool", "bool"),
    }

    @classmethod
    def is_valid_expr(cls, expr):
        return expr[0] in cls.op_codes

    def compile(self, expr):
        opcode = expr[0]
        if not verify_shape(expr, [str, None, None]):
            raise CodegenError(f"opcode {opcode} takes exactly 2 operands")

        input_instr_list, operand_syms, operand_types = [], [], []
        for ex in expr[1:]:
            expr_obj = super().compile(ex)
            allowed_types = self.op_codes[opcode][0]
            match, expected_type = type_match(expr_obj.typ, allowed_types)
            if not match:
                raise CodegenError(f"Operands to {opcode} must be {expected_type}")
            input_instr_list += expr_obj.instructions
            operand_types.append(expr_obj.typ)
            operand_syms.append(expr_obj.symbol)

        if operand_types[0] != operand_types[1]:
            raise CodegenError(f"Operands to `{opcode}` must have same width")

        res_sym = random_label(CLISP_PREFIX)
        res_type = self.op_codes[opcode][1]
        if res_type is None:
            res_type = operand_types[0]

        instructions = input_instr_list + [
            ["set", [res_sym, res_type], [opcode, *operand_syms]]
        ]
        return ExpressionResult(instructions, res_sym, res_type)


class NotExpression(Expression):
    @classmethod
    def is_valid_expr(cls, expr):
        return expr[0] == "not"

    def compile(self, expr):
        if len(expr) != 2:
            raise CodegenError(f"`not` takes only 1 operands")
        input_expr = super().compile(expr[1])
        if input_expr.typ != "bool":
            raise CodegenError(f"Operand to `not` must be bool")

        res_sym = random_label(CLISP_PREFIX)
        instrs = input_expr.instructions + [
            ["set", [res_sym, "bool"], ["not", input_expr.symbol]]
        ]
        return ExpressionResult(instrs, res_sym, "bool")


class PtrAddExpression(Expression):
    @classmethod
    def is_valid_expr(cls, expr):
        return expr[0] == "ptradd"

    def compile(self, expr):
        if len(expr) != 3:
            raise CodegenError(f"Bad ptradd expression: {expr}")

        offset = super().compile(expr[2])
        ptr_name = self.ctx.scoped_lookup(expr[1])
        ptr_type = self.ctx.variable_types[ptr_name]
        res_sym = random_label(CLISP_PREFIX)

        instrs = offset.instructions + [
            ["set", [res_sym, ptr_type], ["ptradd", ptr_name, offset.symbol]]
        ]
        return ExpressionResult(instrs, res_sym, ptr_type)


class StructPtrAddExpression(Expression):
    @classmethod
    def is_valid_expr(cls, expr):
        return expr[0] == "sptradd"

    def is_struct_pointer_type(self, typ):
        return (
            isinstance(typ, list)
            and typ[0] == "ptr"
            and isinstance(typ[1], list)
            and typ[1][0] == "struct"
            and typ[1][1] in self.ctx.struct_types
        )

    def compile(self, expr):
        if not len(expr) == 3:
            raise CodegenError(f"Bad sptradd expression: {expr}")

        struct_ptr = super().compile(expr[1])
        field = expr[2]

        if not self.is_struct_pointer_type(struct_ptr.typ):
            raise CodegenError(f"Not a struct pointer: {expr[1]} {struct_ptr.typ}")

        struct_name = struct_ptr.typ[1][1]
        field_idx, field_type = self.ctx.struct_types[struct_name][field]
        res_sym = random_label(CLISP_PREFIX)
        res_type = ["ptr", field_type]

        instrs = struct_ptr.instructions + [
            [
                "set",
                [res_sym, res_type],
                ["ptradd", struct_ptr.symbol, 0, field_idx],
            ]
        ]
        return ExpressionResult(instrs, res_sym, res_type)


class LoadExpression(Expression):
    @classmethod
    def is_valid_expr(cls, expr):
        return expr[0] == "load"

    def compile(self, expr):
        if len(expr) != 2:
            raise CodegenError(f"Bad load expression: {expr}")

        ptr = super().compile(expr[1])
        res_sym = random_label(CLISP_PREFIX)
        instrs = ptr.instructions + [
            ["set", [res_sym, ptr.typ[1]], ["load", ptr.symbol]]
        ]
        return ExpressionResult(instrs, res_sym, ptr.typ[1])


class StoreExpression(Expression):
    @classmethod
    def is_valid_expr(cls, expr):
        return expr[0] == "store"

    def compile(self, expr):
        if len(expr) != 3:
            raise CodegenError(f"Bad store expression: {expr}")

        ptr = super().compile(expr[1])
        val = super().compile(expr[2])
        if ptr.typ[1] != val.typ:
            # TODO: Error test
            raise CodegenError(f"Cannot store {val.typ} value to {ptr.typ}")

        instrs = [
            *ptr.instructions,
            *val.instructions,
            ["store", ptr.symbol, val.symbol],
        ]
        return ExpressionResult(instrs, val.symbol, val.typ)


class AllocExpression(Expression):
    @classmethod
    def is_valid_expr(cls, expr):
        return expr[0] == "alloc"

    def compile(self, expr):
        if len(expr) != 3:
            raise CodegenError(f"Bad alloc expression: {expr}")

        ptr_type = ["ptr", expr[1]]
        res_sym = random_label(CLISP_PREFIX)
        size = super().compile(expr[2])
        instrs = [
            *size.instructions,
            ["set", [res_sym, ptr_type], ["alloc", size.symbol]],
        ]
        return ExpressionResult(instrs, res_sym, ptr_type)


class CastExpression(Expression):
    cast_ops = {
        ## opcode -> allowed operand types, allowed result type
        ## `"ptr"` indicates pointer, `None` indicates any
        # Type conversion
        "sitofp": ("_int", "_float"),
        "fptosi": ("_float", "_int"),
        "uitofp": ("_int", "_float"),
        "fptoui": ("_float", "_int"),
        "inttoptr": ("_int", "_ptr"),
        "ptrtoint": ("_ptr", "_int"),
        "sext": ("_int", "_int"),
        "zext": ("_int", "_int"),
        "trunc": ("_int", "_int"),
        "fpext": ("_float", "_float"),
        "fptrunc": ("_float", "_float"),
        "bitcast": (None, None),
    }

    @classmethod
    def is_valid_expr(cls, expr):
        return expr[0] in cls.cast_ops

    def compile(self, expr):
        opcode = expr[0]
        if len(expr) != 3:
            raise CodegenError(f"`{opcode}` takes exactly 2 operands")

        operand = super().compile(expr[1])
        res_sym = random_label(CLISP_PREFIX)
        res_type = expr[2]
        op_input_type, op_res_type = self.cast_ops[opcode]
        match, expected_type = type_match(operand.typ, op_input_type)
        if not match:
            raise CodegenError(f"Operand to {opcode} must be {expected_type}")
        match, expected_type = type_match(res_type, op_res_type)
        if not match:
            raise CodegenError(f"{opcode} produces {expected_type}")
        instrs = [
            *operand.instructions,
            ["set", [res_sym, res_type], [opcode, operand.symbol, res_type]],
        ]
        return ExpressionResult(instrs, res_sym, res_type)


class PtrToExpression(Expression):
    @classmethod
    def is_valid_expr(cls, expr):
        return expr[0] == "ptr-to"

    def compile(self, expr):
        if not verify_shape(expr, [str, str]):
            raise CodegenError(f"Bad ptr-to expression: {expr}")

        pointee_sym = self.ctx.scoped_lookup(expr[1])
        pointee_type = self.ctx.variable_types[pointee_sym]
        res_sym = random_label(CLISP_PREFIX)
        instr_list = []
        instr_list.append(
            ["set", [res_sym, ["ptr", pointee_type]], ["ptr-to", pointee_sym]]
        )
        return ExpressionResult(instr_list, res_sym, ["ptr", pointee_type])


def type_match(typ, pattern):
    """
    Verify type `typ` against `pattern`.
    Returns `match`, `expected_type`, where
    - match: whether the type matches
    - expected_type: a description of the expected type(s). Useful for error reporting

    `pattern` can either be an actual type, i.e. "int", ["ptr", "int"], or
    - "_ptr": any pointer
    - "_int": integer type
    - "_float": floating-point type
    - None: any type
    """
    int_types = {"int8", "int16", "int", "int32", "int64"}
    float_types = {"float", "double"}
    if pattern is None:
        return True, ""
    elif pattern == "_int":
        return isinstance(typ, str) and typ in int_types, f"one of {int_types}"
    elif pattern == "_float":
        return isinstance(typ, str) and typ in float_types, f"one of {float_types}"
    elif pattern == "_ptr":
        return typ[0] == "ptr", "a pointer type"
    elif pattern == "_struct":
        return typ[0] == "struct", "a struct type"
    else:
        return typ == pattern, str(pattern)


if __name__ == "__main__":
    brilisp_code_generator = BrilispCodeGenerator()
    json.dump(brilisp_code_generator.c_lisp(json.loads(sys.stdin.read())), sys.stdout)
