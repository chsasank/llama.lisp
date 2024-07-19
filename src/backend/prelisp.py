import sys
import json
import importlib
import os

sys.path.append(os.getcwd())


def prelisp(expr, module_name):
    assert isinstance(module_name, str) and module_name.endswith(".py")
    module = importlib.import_module(module_name[: -len(".py")])
    return preprocess(expr, module)[0]


def preprocess(expr, env):
    if isinstance(expr, list):
        if expr[0] == "unquote":
            assert len(expr) == 2
            return expand_macro(expr[1], env), "append"
        elif expr[0] == 'unquote-splicing':
            assert len(expr) == 2
            return expand_macro(expr[1], env), "extend"
        else:
            res = []
            for x in expr:
                out, mode = preprocess(x, env)
                if mode == 'append':
                    res.append(out)
                else:
                    res.extend(out)
            
            return res, "append"
    else:
        return expr, "append"


def expand_macro(expr, env):
    if isinstance(expr, list):
        fn_name, fn_args = expr[0], expr[1:]
        fn = getattr(env, fn_name)
        expr_out = fn(*fn_args)
        return clean_python_output(expr_out)
    elif isinstance(expr, str):
        var = expr
        expr_out = getattr(env, var)
        return clean_python_output(expr_out)
    else:
        raise Exception("Unknown macro")


def clean_python_output(x):
    return json.loads(json.dumps(x))


def main(mod_name):
    expr = json.load(sys.stdin)
    print(json.dumps(prelisp(expr, mod_name)))


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="What the program does")
    parser.add_argument(
        "macro_module", help="python file with entry for macro functions"
    )
    args = parser.parse_args()
    main(args.macro_module)
