import sys
import json
import importlib


def prelisp(expr):
    assert expr[0] == "prelisp"
    module_name = expr[1]
    assert isinstance(module_name, str) and module_name.endswith(".py")
    # TODO: figure out paths and stuff
    module = importlib.import_module(module_name[: -len(".py")])

    return preprocess(expr[2:], module)


def preprocess(expr, env):
    if isinstance(expr, list):
        if expr[0] == "unquote":
            assert len(expr) == 2
            return expand_macro(expr[1], env)
        else:
            return [preprocess(x, env) for x in expr]
    else:
        return expr


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


def main():
    expr = json.load(sys.stdin)
    print(json.dumps(prelisp(expr)))


if __name__ == "__main__":
    main()
