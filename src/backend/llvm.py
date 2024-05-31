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
import random
import string


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
        # names to stack addresses allocated using `alloca` instruction
        self.func_alloca_symtab = {}  # symbol name -> memory address

        # Manages all labels in a function
        self.func_bbs = {}

    def generate(self, bril_prog):
        for fn in bril_prog.functions:
            self.gen_function(fn)

    def gen_type(self, type):
        if isinstance (type, dict):
            if 'ptr' in type:
                return self.gen_type(type['ptr'])
            else:
                raise CodegenError (f"Unknown type {type}")
        elif type == "int":
            return ir.IntType(32)
        elif type == "void":
            return ir.VoidType()
        elif type == "bool":
            return ir.IntType(1)
        else:
            raise CodegenError(f"Unknown type {type}")

    def gen_var(self, var):
        try:
            return self.gen_symbol_load(var)
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
            old_bb = self.builder.block
            new_bb = self.gen_label(instr.label)
            self.builder.function.basic_blocks.append(new_bb)
            if not old_bb.is_terminated:
                # Make control fallthroughs explicit
                self.builder.branch(new_bb)
            self.builder.position_at_start(new_bb)

        def gen_jmp(instr):
            self.builder.branch(self.gen_label(instr.labels[0]))

        def gen_br(instr):
            self.builder.cbranch(
                cond=self.gen_var(instr.args[0]),
                truebr=self.gen_label(instr.labels[0]),
                falsebr=self.gen_label(instr.labels[1]),
            )

        def gen_call(instr):
            callee_func = self.module.globals.get(instr.funcs[0], None)
            if callee_func is None or not isinstance(callee_func, ir.Function):
                raise CodegenError("Call to unknown function", dict(instr))
            if len(callee_func.args) != len(instr.args):
                raise CodegenError("Call argument length mismatch", dict(instrs))

            call_args = [self.gen_var(arg) for arg in instr.args]

            if instr.type == "void":
                self.builder.call(callee_func, call_args, name=instr.dest)
            else:
                self.declare_var(self.gen_type(instr.type), instr.dest)
                self.gen_symbol_store(
                    instr.dest,
                    self.builder.call(callee_func, call_args, name=instr.dest),
                )

        def gen_ret(instr):
            if instr.args:
                self.builder.ret(self.gen_var(instr.args[0]))
            else:
                self.builder.ret_void()

        def gen_const(instr):
            self.declare_var(self.gen_type(instr.type), instr.dest)
            self.gen_symbol_store(
                instr.dest, ir.Constant(self.gen_type(instr.type), instr.value)
            )

        def gen_value(instr):
            self.declare_var(self.gen_type(instr.type), instr.dest)
            llvm_instr = getattr(self.builder, value_ops[instr.op])
            self.gen_symbol_store(
                instr.dest,
                llvm_instr(*[self.gen_var(arg) for arg in instr.args], name=instr.dest),
            )

        def gen_comp(instr):
            self.declare_var(self.gen_type(instr.type), instr.dest)
            self.gen_symbol_store(
                instr.dest,
                self.builder.icmp_signed(
                    cmpop=cmp_ops[instr.op],
                    lhs=self.gen_var(instr.args[0]),
                    rhs=self.gen_var(instr.args[1]),
                    name=instr.dest,
                ),
            )

        def gen_alloc(instr):
            pointee_type = self.gen_type(instr.type)
            self.declare_var(pointee_type.as_pointer(), instr.dest)
            self.gen_symbol_store(instr.dest, self.builder.alloca(pointee_type, size=self.gen_var(instr.args[0])))

        def gen_store(instr):
            ptr = self.gen_symbol_load(instr.args[0])
            self.builder.store(self.gen_var(instr.args[1]), ptr)

        def gen_load(instr):
            self.declare_var(self.gen_type(instr.type), instr.dest)
            ptr = self.gen_symbol_load(instr.args[0])
            self.gen_symbol_store(
                instr.dest, self.builder.load(ptr))

        for instr in instrs:
            if "label" in instr:
                gen_label(instr)
            elif self.builder.block.is_terminated:
                pass  # Do not codegen for unreachable code
            elif instr.op == "nop":
                pass
            elif instr.op == "jmp":
                gen_jmp(instr)
            elif instr.op == "br":
                gen_br(instr)
            elif instr.op == "call":
                gen_call(instr)
            elif instr.op == "ret":
                gen_ret(instr)
            elif instr.op == "const":
                gen_const(instr)
            elif instr.op == "alloc":
                gen_alloc(instr)
            elif instr.op == "store":
                gen_store(instr)
            elif instr.op == "load":
                gen_load(instr)
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

        return func

    def declare_var(self, typ, name):
        """Allocate a pointer using alloca and add it to the symbol table, if it doesn't already exist"""
        if (name not in self.func_alloca_symtab) or (
            self.func_alloca_symtab[name].type != typ.as_pointer()
        ):
            # This is either a new variable or the same variable with a new type
            builder = ir.IRBuilder(
                self.func_alloca_bb
            )  # Use a separate builder so we don't mess with the global builder's position
            self.func_alloca_symtab[name] = builder.alloca(typ, name=name)

    def gen_symbol_load(self, name):
        if name in self.func_alloca_symtab:
            return self.builder.load(self.func_alloca_symtab[name])
        else:
            raise CodegenError(f"Unknown variable: {name}")

    def gen_symbol_store(self, name, val):
        if name in self.func_alloca_symtab:
            self.builder.store(val, self.func_alloca_symtab[name])
        else:
            raise CodegenError(f"Unknown variable: {name}")

    def gen_function(self, fn):
        # Reset the symbol and labels table.
        # Prototype generation will pre-populate it with function arguments.
        self.func_bbs = {}
        self.func_alloca_symtab = {}
        # Create the function skeleton from the prototype.
        func = self.gen_function_prototype(fn)

        if fn.instrs:
            # Create the entry BB in the function and set the builder to it.
            if "label" in fn.instrs[0]:
                entry_label = fn.instrs[0].label
                fn_instrs = fn.instrs[1:]
            else:
                entry_label = random_label("entry")
                fn_instrs = fn.instrs

            # Create a basic block for allocas, and let it be the actual entry point
            # Note: using `ir.Builder.position_at_start(block)` doesn't seem to work for some reason
            alloca_label = random_label("alloca")
            self.func_alloca_bb = func.append_basic_block(alloca_label)
            bb_entry = func.append_basic_block(entry_label)
            self.func_bbs[entry_label] = bb_entry
            self.builder = ir.IRBuilder(bb_entry)

            # Set function argument names from AST
            for i, arg in enumerate(func.args):
                arg.name = fn.args[i].name
                self.declare_var(self.gen_type(fn.args[i].type), arg.name)
                self.gen_symbol_store(fn.args[i].name, arg)

            # Function body
            self.gen_instructions(fn_instrs)
            self.builder.position_at_end(self.func_alloca_bb)
            self.builder.branch(bb_entry)  # Cannot use implicit fallthroughs
        return func


def main():
    bril_prog = munch.munchify(json.load(sys.stdin))
    code_gen = LLVMCodeGenerator()
    code_gen.generate(bril_prog)
    print(code_gen.module)


def random_label(prefix="", length=10):
    """
    Return a random string of the form `prefix_XXXXXXXXXX`
    """
    return (
        prefix
        + "_"
        + "".join([random.choice(string.ascii_letters) for i in range(length)])
    )


if __name__ == "__main__":
    main()
