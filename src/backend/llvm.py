"""
LLVM Code generator

References:
1. https://github.com/eliben/pykaleidoscope/
2. https://llvm.org/docs/tutorial/
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

    def generate(self, bril_prog):
        for fn in bril_prog.functions:
            self.gen_function(fn)

    def gen_function(self, fn):
        # Reset the symbol table. Prototype generation will pre-populate it with
        # function arguments.
        self.func_symtab = {}
        # Create the function skeleton from the prototype.
        func = self.gen_function_prototype(fn)
        # Create the entry BB in the function and set the builder to it.
        bb_entry = func.append_basic_block("entry")
        self.builder = ir.IRBuilder(bb_entry)
        retval = self.gen_instructions(fn.instrs)
        self.builder.ret(retval)
        return func

    def gen_function_prototype(self, fn):
        funcname = fn.name

        # Create a function type
        func_ty = ir.FunctionType(
            self.gen_type(fn.type), [self.gen_type(arg.type) for arg in fn.args]
        )

        # If a function with this name already exists in the module...
        if funcname in self.module.globals:
            # We only allow the case in which a declaration exists and now the
            # function is defined (or redeclared) with the same number of args.
            existing_func = self.module[funcname]
            if not isinstance(existing_func, ir.Function):
                raise CodegenError("Function/Global name collision", funcname)
            if not existing_func.is_declaration():
                raise CodegenError("Redifinition of {0}".format(funcname))
            if len(existing_func.function_type.args) != len(func_ty.args):
                raise CodegenError("Redifinition with different number of arguments")
            func = self.module.globals[funcname]
        else:
            # Otherwise create a new function
            func = ir.Function(self.module, func_ty, funcname)
        # Set function argument names from AST
        for i, arg in enumerate(func.args):
            arg.name = fn.args[i].name
            self.func_symtab[arg.name] = arg

        return func

    def gen_type(self, type):
        if type == "int":
            return ir.IntType(32)
        elif type == "void":
            return ir.VoidType()
        else:
            raise CodegenError(f"Unknown type {type}")

    def gen_instructions(self, instrs):
        """Generate instructions from body and return final symbol to be returned"""
        value_ops = {
            "add": "add",
            "mul": "mul",
            "sub": "sub",
            "div": "sdiv",
        }

        cmp_ops = {
            "eq": "==",
            "lt": "<",
            "gt": ">",
            "le": "<=",
            "ge": ">=",
            "neq": "!=",
        }

        for instr in instrs:
            if instr.op == "const":
                self.func_symtab[instr.dest] = ir.Constant(
                    self.gen_type(instr.type), instr.value
                )
            elif instr.op in value_ops:
                llvm_instr = getattr(self.builder, value_ops[instr.op])
                self.func_symtab[instr.dest] = llvm_instr(
                    *[self.gen_var(arg) for arg in instr.args], name=instr.dest
                )
            elif instr.op in cmp_ops:
                self.func_symtab[instr.dest] = self.builder.icmp_signed(
                    cmpop=cmp_ops[instr.op],
                    lhs=self.gen_var(instr.args[0]),
                    rhs=self.gen_var(instr.args[1]),
                    name=instr.dest,
                )
            elif instr.op == "call":
                callee_func = self.module.globals.get(instr.funcs[0], None)
                if callee_func is None or not isinstance(callee_func, ir.Function):
                    raise CodegenError("Call to unknown function", dict(instr))
                if len(callee_func.args) != len(instr.args):
                    raise CodegenError("Call argument length mismatch", dict(instrs))

                call_args = [self.gen_var(arg) for arg in instr.args]

                self.func_symtab[instr.dest] = self.builder.call(
                    callee_func, call_args, name=instr.dest
                )

            elif instr.op == "ret":
                return self.func_symtab[instr.args[0]]
            else:
                raise CodegenError(f"Unknown op in the instruction: {dict(instr)}")

    def gen_var(self, var):
        try:
            return self.func_symtab[var]
        except KeyError:
            raise CodegenError("Unknown variable", var)


def main():
    bril_prog = munch.munchify(json.load(sys.stdin))
    code_gen = LLVMCodeGenerator()
    code_gen.generate(bril_prog)
    print(code_gen.module)


if __name__ == "__main__":
    main()
