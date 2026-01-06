#!/bin/bash
set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 <file.ksu>"
    exit 1
fi

INPUT="$1"
BASENAME="$(basename "${INPUT%.ksu}")"
CFILE="/tmp/${BASENAME}.c"
EXEC="/tmp/${BASENAME}"

echo "==> Building ksu compiler..."
dune build

echo "==> Compiling $INPUT to C..."
dune exec ksu "$INPUT" > "$CFILE"

echo "==> Formatting $CFILE..."
clang-format -i "$CFILE"
echo "Generated C file: $CFILE"

echo "==> Compiling C code with gcc..."
gcc -o "$EXEC" "$CFILE" src/Runtime/ksu_runtime.c -I src/Runtime

echo "==> Running $EXEC..."
echo "--- OUTPUT ---"
"$EXEC"
