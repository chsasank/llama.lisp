"""
LLVM Code generator

References:
1. https://github.com/eliben/pykaleidoscope/
2. https://llvm.org/docs/tutorial/

Assumes BRIL is in SSA form
"""

import sys
import json
import munch
import llvmlite.ir as ir


class CodegenError(Exception):
    pass


class LLVMCodeGenerator(object):
    def __init__(self):
        """Initialize the code generator.

        This creates a new LLVM module into which code is generated. The
        generate_code() method can be called multiple times. It adds the code
        generated for this node into the module, and returns the IR value for
        the node.

        At any time, the current LLVM module being constructed can be obtained
        from the module attribute.
        """
        self.module = ir.Module()
        self.builder = None

        # Manages a symbol table while a function is being codegen'd. Maps var
        # names to ir.Value.
        self.func_symtab = {}

        # Manages all labels in a function
        self.func_bbs = {}

    def generate(self, bril_prog):
        for fn in bril_prog.functions:
            self.gen_function(fn)

    def gen_type(self, type):
        if type == "int":
            return ir.IntType(32)
        elif type == "void":
            return ir.VoidType()
        elif type == "bool":
            return ir.IntType(1)
        else:
            raise CodegenError(f"Unknown type {type}")

    def gen_var(self, var):
        try:
            return self.func_symtab[var]
        except KeyError:
            raise CodegenError("Unknown variable", var)

    def gen_label(self, label):
        if label not in self.func_bbs:
            self.func_bbs[label] = ir.Block(self.builder.function, label)

        return self.func_bbs[label]

    def gen_instructions(self, instrs):
        """Generate instructions from body and return final symbol to be returned"""
        value_ops = {
            "add": "add",
            "mul": "mul",
            "sub": "sub",
            "div": "sdiv",
            "not": "not_",
            "and": "and_",
            "or": "or_",
        }

        cmp_ops = {
            "eq": "==",
            "lt": "<",
            "gt": ">",
            "le": "<=",
            "ge": ">=",
            "neq": "!=",
        }

        def gen_label(instr):
            bb = self.gen_label(instr.label)
            self.builder.function.basic_blocks.append(bb)
            self.builder.position_at_start(bb)

        def gen_jmp(instr):
            self.builder.branch(self.gen_label(instr.labels[0]))

        def gen_br(instr):
            self.builder.cbranch(
                cond=self.gen_var(instr.args[0]),
                truebr=self.gen_label(instr.labels[0]),
                falsebr=self.gen_label(instr.labels[1]),
            )

        def gen_phi(instr):
            phi = self.builder.phi(self.gen_type(instr.type), name=instr.dest)
            for i in range(len(instr.args)):
                phi.add_incoming(
                    value=self.gen_var(instr.args[i]),
                    block=self.gen_label(instr.labels[i]),
                )
            self.func_symtab[instr.dest] = phi

        def gen_call(instr):
            callee_func = self.module.globals.get(instr.funcs[0], None)
            if callee_func is None or not isinstance(callee_func, ir.Function):
                raise CodegenError("Call to unknown function", dict(instr))
            if len(callee_func.args) != len(instr.args):
                raise CodegenError("Call argument length mismatch", dict(instrs))

            call_args = [self.gen_var(arg) for arg in instr.args]

            self.func_symtab[instr.dest] = self.builder.call(
                callee_func, call_args, name=instr.dest
            )

        def gen_ret(instr):
            if instr.args:
                self.builder.ret(self.gen_var(instr.args[0]))
            else:
                self.builder.ret_void()

        def gen_const(instr):
            self.func_symtab[instr.dest] = ir.Constant(
                self.gen_type(instr.type), instr.value
            )

        def gen_value(instr):
            llvm_instr = getattr(self.builder, value_ops[instr.op])
            self.func_symtab[instr.dest] = llvm_instr(
                *[self.gen_var(arg) for arg in instr.args], name=instr.dest
            )

        def gen_comp(instr):
            self.func_symtab[instr.dest] = self.builder.icmp_signed(
                cmpop=cmp_ops[instr.op],
                lhs=self.gen_var(instr.args[0]),
                rhs=self.gen_var(instr.args[1]),
                name=instr.dest,
            )

        for instr in instrs:
            if "label" in instr:
                gen_label(instr)
            elif instr.op == "jmp":
                gen_jmp(instr)
            elif instr.op == "br":
                gen_br(instr)
            elif instr.op == "phi":
                gen_phi(instr)
            elif instr.op == "call":
                gen_call(instr)
            elif instr.op == "ret":
                gen_ret(instr)
            elif instr.op == "const":
                gen_const(instr)
            elif instr.op in value_ops:
                gen_value(instr)
            elif instr.op in cmp_ops:
                gen_comp(instr)
            else:
                raise CodegenError(f"Unknown op in the instruction: {dict(instr)}")

    def gen_function_prototype(self, fn):
        funcname = fn.name

        # Create a function type
        func_ty = ir.FunctionType(
            self.gen_type(fn.type),
            [self.gen_type(arg.type) for arg in fn.get("args", [])],
        )

        # If a function with this name already exists in the module...
        if funcname in self.module.globals:
            # We only allow the case in which a declaration exists and now the
            # function is defined (or redeclared) with the same number of args.
            existing_func = self.module.globals[funcname]
            if not isinstance(existing_func, ir.Function):
                raise CodegenError("Function/Global name collision", funcname)
            if not existing_func.is_declaration:
                raise CodegenError("Redifinition of {0}".format(funcname))
            if len(existing_func.function_type.args) != len(func_ty.args):
                raise CodegenError("Redifinition with different number of arguments")
            func = existing_func
        else:
            # Otherwise create a new function
            func = ir.Function(self.module, func_ty, funcname)
        # Set function argument names from AST
        for i, arg in enumerate(func.args):
            arg.name = fn.args[i].name
            self.func_symtab[fn.args[i].name] = arg

        return func

    def gen_function(self, fn):
        # Reset the symbol and labels table.
        # Prototype generation will pre-populate it with function arguments.
        self.func_symtab = {}
        self.func_bbs = {}
        # Create the function skeleton from the prototype.
        func = self.gen_function_prototype(fn)

        if fn.instrs:
            # Create the entry BB in the function and set the builder to it.
            assert "label" in fn.instrs[0]
            entry_label = fn.instrs[0].label
            bb_entry = func.append_basic_block(entry_label)
            self.func_bbs[entry_label] = bb_entry
            self.builder = ir.IRBuilder(bb_entry)
            self.gen_instructions(fn.instrs[1:])
        return func


def main():
    bril_prog = munch.munchify(json.load(sys.stdin))
    code_gen = LLVMCodeGenerator()
    code_gen.generate(bril_prog)
    print(code_gen.module)


if __name__ == "__main__":
    main()
