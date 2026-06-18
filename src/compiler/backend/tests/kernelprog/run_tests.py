#!/usr/bin/env python3
"""Run all kernelprog GPU tests and optionally update snapshot expectations."""

import argparse
import glob
import os
import subprocess
import sys


def run_test(test_dir, update=False):
    test_py = os.path.join(test_dir, "test.py")
    expected = os.path.join(test_dir, "test.expected")
    if not os.path.exists(test_py):
        return None

    name = os.path.basename(test_dir)
    proc = subprocess.run(
        ["python", "../kernel_runner.py", "test.py"],
        cwd=test_dir,
        capture_output=True,
        text=True,
    )
    out = proc.stdout
    err = proc.stderr

    if proc.returncode != 0:
        print(f"{name}: FAILED (runner exited {proc.returncode})")
        if out:
            print(out)
        if err:
            print(err, file=sys.stderr)
        return False

    if update:
        with open(expected, "w") as f:
            f.write(out)
        print(f"{name}: updated snapshot")
        return True

    if os.path.exists(expected):
        with open(expected) as f:
            exp = f.read()
        if out == exp:
            print(f"{name}: PASSED (snapshot)")
            return True
        else:
            print(f"{name}: FAILED (snapshot mismatch)")
            print("--- expected ---")
            print(exp, end="")
            print("--- actual ---")
            print(out, end="")
            return False

    print(out, end="")
    return True


def main():
    parser = argparse.ArgumentParser(
        description="Run kernelprog GPU tests."
    )
    parser.add_argument(
        "--snapshot", action="store_true",
        help="Update test.expected snapshot files instead of comparing."
    )
    parser.add_argument(
        "tests", nargs="*",
        help="Specific test directories to run (default: all with test.py)."
    )
    args = parser.parse_args()

    root = os.path.dirname(os.path.abspath(__file__))
    if args.tests:
        dirs = [os.path.join(root, t) for t in args.tests]
    else:
        dirs = sorted(
            d for d in glob.glob(os.path.join(root, "*"))
            if os.path.isdir(d) and os.path.exists(os.path.join(d, "test.py"))
        )

    results = []
    for d in dirs:
        result = run_test(d, args.snapshot)
        if result is not None:
            results.append(result)

    if not results:
        print("No tests found.")
        sys.exit(1)

    passed = sum(results)
    total = len(results)
    print(f"\n{passed}/{total} tests passed")
    sys.exit(0 if passed == total else 1)


if __name__ == "__main__":
    main()
