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
    if isinstance(expr, list) and len(expr) > 0:
        if expr[0] == "unquote":
            assert len(expr) == 2
            return expand_macro(expr[1], env), "append"
        elif expr[0] == "unquote-splicing":
            assert len(expr) == 2
            out = expand_macro(expr[1], env)
            assert isinstance(out, list)
            return out, "extend"
        else:
            res = []
            for x in expr:
                out, mode = preprocess(x, env)
                if mode == "append":
                    res.append(out)
                else:
                    res.extend(out)

            return res, "append"
    else:
        return expr, "append"


def is_keyword_arg(expr):
    if (
        isinstance(expr, list)
        and len(expr)
        and isinstance(expr[0], str)
        and expr[0].startswith("#:")
    ):
        assert len(expr) == 2, f"Invalid keyword argument: {expr}"
        return True
    else:
        return False


def expand_macro(expr, env):
    if isinstance(expr, list):
        fn_name, fn_args = expr[0], expr[1:]
        fn_name = fn_name.replace("-", "_")
        fn = getattr(env, fn_name)

        fn_posargs, fn_kwargs = [], {}
        for arg in fn_args:
            if is_keyword_arg(arg):
                argname = arg[0][2:]  # Strip the "#:"
                argval = arg[1]
                fn_kwargs[argname] = preprocess(argval, env)[0]
            else:
                fn_posargs.append(preprocess(arg, env)[0])

        expr_out = fn(*fn_posargs, **fn_kwargs)
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
