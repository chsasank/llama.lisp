#!/usr/bin/env bash
# Run all compiler backend tests for llama.lisp.
# This script must be invoked from the directory it lives in, or pass the
# directory as the first argument.

set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_DIR="${1:-$SCRIPT_DIR}"

cd "$TEST_DIR" || { echo "Failed to cd to $TEST_DIR"; exit 1; }

failures=0

run_turnt() {
    local name="$1"
    local dir="$2"
    local pattern="$3"

    echo "=== $name ==="
    if [ -d "$dir" ]; then
        (cd "$dir" && turnt $pattern)
        local status=$?
        if [ $status -ne 0 ]; then
            echo "FAILURE: $name tests failed (exit $status)"
            failures=$((failures + 1))
        fi
    else
        echo "SKIP: $dir not found"
    fi
}

# Tests with turnt.toml configurations.
run_turnt "brilisp" "brilisp" "./*.sexp"
run_turnt "c-lisp" "c-lisp" "./*.sexp"
run_turnt "prelisp" "prelisp" "./*.sexp"

# Tests driven by inline ";; CMD:" comments.
run_turnt "parser" "parser" "*.sexp"
run_turnt "cuda-lisp" "cuda-lisp" "*.sexp"

# GPU kernel programming tests.
echo "=== kernelprog ==="
if [ -d "kernelprog" ]; then
    (cd "kernelprog" && python run_tests.py)
    status=$?
    if [ $status -ne 0 ]; then
        echo "FAILURE: kernelprog tests failed (exit $status)"
        failures=$((failures + 1))
    fi
else
    echo "SKIP: kernelprog not found"
fi

if [ $failures -eq 0 ]; then
    echo
    echo "All compiler backend tests passed."
    exit 0
else
    echo
    echo "$failures compiler backend test suite(s) failed."
    exit 1
fi
