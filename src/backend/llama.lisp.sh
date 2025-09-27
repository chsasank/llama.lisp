#!/bin/sh

### Driver for C-Lisp compiler components ###

set -o pipefail
set -e
SCRIPT_DIR=`dirname $0`

## Wrappers around compiler components/stages
# Each function here takes input from standard input

# C-Lisp to Brilisp compilation
clisp () {
    guile ${SCRIPT_DIR}/utils/sexp-json.scm | python3 ${SCRIPT_DIR}/c-lisp.py | guile ${SCRIPT_DIR}/utils/json-sexp.scm
}

# Brilisp to LLVM compilation
brilisp () {
    guile ${SCRIPT_DIR}/utils/sexp-json.scm | python3 ${SCRIPT_DIR}/brilisp.py | python ${SCRIPT_DIR}/llvm.py
}

# LLVM to executable (offloaded to clang)
llvm () {
    tmp_in=`mktemp --suffix '.ll'`
    cat > now.ll
    clang now.ll $CLANG_ARGS
    rm $tmp_in
}

stage_wrap () {
    echo "STAGE ERROR" $ERR >&2
    if [ -z $ERR ]; then
        $1
        ERR=$?
    fi
}

# Driver
main () {
    # Assume input is C-Lisp, and user wants to compile and run
    in=`mktemp --suffix ".in"`
    out=`mktemp --suffix ".out"`
    cat > $in
    for stage in clisp brilisp; do
        $stage < $in > $out
        cp $out $in
    done
    llvm < $in
}

usage () {
cat << EOF
${1:-C-Lisp compiler driver}

Usage: $0 -i <input filename> [ -s <stage> ] [ -c <clang options> ] [ -h ]
Options:
  -i: Specify the source file
  -s: Specify the stage to run
      supported stages: clisp, brilisp, llvm
  -c: Supply arguments to clang (note that you will have to quote
      them if there are multiple arguments)
      Defaults to: '<script directory>/tests/brilisp/runtime.c -Wno-override-module -O2'
  -h: Display this message
EOF
exit
}

while getopts "i:s:c:h" opt; do
    case $opt in
    i) input="$OPTARG";;
    s) stage="$OPTARG";;
    c) clang_args="$OPTARG";;
    h) usage;;
    esac
done


if [ -z $input ]; then
    usage 'Please specify input with `-i <filename>`'
    exit 1
fi

if [ -z "$clang_args" ]; then
    CLANG_ARGS="${SCRIPT_DIR}/tests/brilisp/runtime.c -Wno-override-module -O2"
else
    CLANG_ARGS="$clang_args"
fi

if [ ! -z $stage ]; then
    echo 'Running stage' $stage >&2
    cat $input | ${stage}
else
    cat $input | main
fi
