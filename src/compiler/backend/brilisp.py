import json
import sys


def is_list(x):
    return isinstance(x, list)


def is_struct(expr):
    return isinstance(expr, list) and (expr[0] == "define-struct")


def is_global(expr):
    return isinstance(expr, list) and (expr[0] == "define-global")


def gen_struct(expr):
    return {
        "name": expr[1],
        "elements": [gen_type(typ) for typ in expr[2]],
    }


def gen_globals(expr):
    return {
        "name": expr[1][0],
        "type": gen_type(expr[1][1]),
        "element": expr[2],
    }


def is_function(expr):
    return isinstance(expr, list) and (expr[0] == "define")


def is_string(expr):
    return isinstance(expr, list) and (expr[0] == "define-string")


def gen_string(expr):
    assert expr[2][0] == "string"
    return {
        "name": expr[1],
        "value": expr[2][1],
    }


def gen_function(expr):
    header = expr[1]
    name = header[0][0]
    typ = header[0][1]
    args = header[1:]
    instrs = expr[2:]
    return {
        "name": name,
        "type": gen_type(typ),
        "args": [gen_arg(x) for x in args],
        "instrs": [gen_instr(x) for x in instrs],
    }


def gen_type(typ):
    if is_list(typ):
        if typ[0] == "ptr":
            retval = {"ptr": gen_type(typ[1])}
            if len(typ) > 2 and is_list(typ[2]):
                assert typ[2][0] == "addrspace"
                retval["addrspace"] = typ[2][1]
            return retval
        elif typ[0] == "struct":
            return {"struct": gen_type(typ[1])}
        else:
            raise Exception("Invalid type: {typ}")
    else:
        return typ


def gen_arg(arg):
    return {"name": arg[0], "type": gen_type(arg[1])}


def gen_instr(instr):
    def is_const(instr):
        return (instr[0] == "set") and (instr[2][0] == "const")

    def gen_constr_instr(instr):
        return {
            "op": "const",
            "type": gen_type(instr[1][1]),
            "dest": instr[1][0],
            "value": instr[2][1],
        }

    def is_value(instr):
        value_op = {
            # Integer arithmetic
            "add",
            "mul",
            "sub",
            "div",
            "rem",
            # Integer comparison
            "eq",
            "ne",
            "lt",
            "gt",
            "le",
            "ge",
            # Boolean arithmetic
            "not",
            "and",
            "or",
            # Pointer operations
            "alloc",
            "load",
            "ptradd",
            "ptr-to",
            "id",
            # Floating-point arithmetic
            "fadd",
            "fsub",
            "fmul",
            "fdiv",
            # Floating-point comparison
            "feq",
            "fne",
            "flt",
            "fgt",
            "fle",
            "fge",
            # Type conversion
            "sitofp",
            "fptosi",
            "uitofp",
            "fptoui",
            "inttoptr",
            "ptrtoint",
            "sext",
            "zext",
            "trunc",
            "fpext",
            "fptrunc",
            "bitcast",
            # String reference
            "string-ref",
            "addrspacecast",
        }
        return (instr[0] == "set") and (instr[2][0] in value_op)

    def gen_value_instr(instr):
        return {
            "op": instr[2][0],
            "type": gen_type(instr[1][1]),
            "dest": instr[1][0],
            "args": instr[2][1:],
        }

    def is_ret(instr):
        return instr[0] == "ret"

    def gen_ret_instr(instr):
        return {"op": "ret", "args": instr[1:]}

    def is_call(instr):
        return (instr[0] == "set") and (instr[2][0] == "call")

    def gen_call_instr(instr):
        return {
            "op": "call",
            "type": gen_type(instr[1][1]),
            "dest": instr[1][0],
            "funcs": [instr[2][1]],
            "args": instr[2][2:],
        }

    def is_jmp(instr):
        return instr[0] == "jmp"

    def gen_jmp_instr(instr):
        return {"op": "jmp", "labels": instr[1:]}

    def is_label(instr):
        return instr[0] == "label"

    def gen_label_instr(instr):
        return {"label": instr[1]}

    def is_br(instr):
        return instr[0] == "br"

    def gen_br_instr(instr):
        return {"op": "br", "args": [instr[1]], "labels": instr[2:]}

    def is_nop(instr):
        return instr[0] == "nop"

    def gen_nop_instr(instr):
        return {"op": "nop"}

    def is_store(instr):
        return instr[0] == "store"

    def gen_store_instr(instr):
        return {"op": "store", "args": instr[1:]}

    if is_const(instr):
        return gen_constr_instr(instr)
    elif is_value(instr):
        return gen_value_instr(instr)
    elif is_ret(instr):
        return gen_ret_instr(instr)
    elif is_call(instr):
        return gen_call_instr(instr)
    elif is_jmp(instr):
        return gen_jmp_instr(instr)
    elif is_label(instr):
        return gen_label_instr(instr)
    elif is_br(instr):
        return gen_br_instr(instr)
    elif is_nop(instr):
        return gen_nop_instr(instr)
    elif is_store(instr):
        return gen_store_instr(instr)
    else:
        raise SyntaxError(f"Unknown instruction {instr}")


def brilisp(expr):
    assert expr[0] == "brilisp"
    body = expr[1:]
    functions, strings, structs, globals = [], [], [], []
    for x in body:
        if is_function(x):
            functions.append(gen_function(x))
        elif is_string(x):
            strings.append(gen_string(x))
        elif is_struct(x):
            structs.append(gen_struct(x))
        elif is_global(x):
            globals.append(gen_globals(x))
        else:
            raise Exception(f"{x} is neither function nor string nor struct")
    return {
        "functions": functions,
        "strings": strings,
        "structs": structs,
        "globals": globals,
    }


def main():
    expr = json.load(sys.stdin)
    print(json.dumps(brilisp(expr)))


if __name__ == "__main__":
    main()
