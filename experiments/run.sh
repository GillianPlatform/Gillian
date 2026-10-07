#!/usr/bin/env bash
# Run the corpus of the paper's Sec. 7.2: 32 Gillian verification runs with
# --certified-smt, each of which sends every SMT query to both Gillian's own
# encoder and the verified one, and logs both.
#
# Usage: experiments/run.sh [-o OUT_DIR] [-t SECONDS] [CASE...]
#
#   -o OUT_DIR   where the run goes (default: experiments/results). Cleared
#                first, so that it always holds exactly one run.
#   -t SECONDS   per-case timeout (default: 120)
#   CASE...      only the cases whose id starts with one of these, e.g.
#                `run.sh 00 23-wisl`; default: all
#
# OUT_DIR ends up with the same layout as paper-run/:
#   queries.jsonl   one record per SMT query (see README.md)
#   status.tsv      per case: id, exit code, expected exit codes, seconds
#   logs/           each case's stdout (.out) and stderr (.err)
#
# Exits 0 when every case exits as expected, 1 otherwise.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT="$ROOT/experiments/results"
TIMEOUT_SECONDS=120
while getopts "o:t:" opt; do
  case "$opt" in
    o) OUT="$(mkdir -p "$OPTARG" && cd "$OPTARG" && pwd)" ;;
    t) TIMEOUT_SECONDS="$OPTARG" ;;
    *) sed -n '2,19p' "$0" >&2; exit 2 ;;
  esac
done
shift $((OPTIND - 1))
SELECT=("$@")

# Gillian runs from the repository root, where it finds its runtime files.
cd "$ROOT"

# Build up front, so that no case's timeout is spent building.
dune build --display=quiet 2>&1 | grep -v "^ld: warning" || true

rm -rf "$OUT/logs"
mkdir -p "$OUT/logs"
: > "$OUT/queries.jsonl"
: > "$OUT/status.tsv"
export GILLIAN_CERTIFIED_SMT_LOG="$OUT/queries.jsonl"

UNEXPECTED=0

# case ID EXPECTED COMMAND...
#
# EXPECTED lists the acceptable exit codes, separated by `|`. A case that does
# not finish within the timeout exits 124; the two Amazon `main` cases may or
# may not finish, depending on the machine, and every query they produce before
# the timeout counts.
case_() {
  local id="$1" expected="$2"
  shift 2
  if [ ${#SELECT[@]} -gt 0 ]; then
    local s match=0
    for s in "${SELECT[@]}"; do [[ "$id" == "$s"* ]] && match=1; done
    [ "$match" -eq 1 ] || return 0
  fi
  local start=$SECONDS status
  set +e
  timeout "$TIMEOUT_SECONDS" dune exec --no-build --display=quiet -- "$@" \
    > "$OUT/logs/$id.out" 2> "$OUT/logs/$id.err"
  status=$?
  set -e
  local verdict=ok
  [[ "|$expected|" == *"|$status|"* ]] || { verdict="UNEXPECTED"; UNEXPECTED=1; }
  printf "%s\t%s\t%s\t%s\n" "$id" "$status" "$expected" "$((SECONDS - start))" \
    >> "$OUT/status.tsv"
  printf "%-32s exit %-3s (expected %-5s) %4ss  %s\n" \
    "$id" "$status" "$expected" "$((SECONDS - start))" "$verdict"
}

JS=./Gillian-JS/Examples
C=./Gillian-C/examples
AMZ_C=(
  $C/amazon/header.c $C/amazon/edk.c $C/amazon/array_list.c $C/amazon/ec.c
  $C/amazon/byte_buf.c $C/amazon/hash_table.c $C/amazon/string.c
  $C/amazon/allocator.c $C/amazon/error.c $C/amazon/base.c
)

# Exit 1: Gillian reports a failed proof, with or without --certified-smt: the
# Amazon byte-cursor case is a deliberate bug; the others fail on this version
# of Gillian. Exit 125: Gillian raises an internal error ("Could not testify
# lemma AppendFieldCC"), also with or without --certified-smt.

case_ 00-js-JaVerT-BST            0 gillian-js verify --certified-smt --dump-smt $JS/JaVerT/BST.js
case_ 01-js-JaVerT-DLL            0 gillian-js verify --certified-smt --dump-smt $JS/JaVerT/DLL.js
case_ 02-js-JaVerT-ExprEval       0 gillian-js verify --certified-smt --dump-smt $JS/JaVerT/ExprEval.js
case_ 03-js-JaVerT-IDGen          0 gillian-js verify --certified-smt --dump-smt $JS/JaVerT/IDGen.js
case_ 04-js-JaVerT-PriQ           0 gillian-js verify --certified-smt --dump-smt $JS/JaVerT/PriQ.js
case_ 05-js-JaVerT-SLL            0 gillian-js verify --certified-smt --dump-smt $JS/JaVerT/SLL.js
case_ 06-js-JaVerT-Sort           0 gillian-js verify --certified-smt --dump-smt $JS/JaVerT/Sort.js
case_ 07-js-JaVerT-annotated-SLL  0 gillian-js verify --certified-smt --dump-smt $JS/JaVerT/annotated/SLL.js
case_ 08-js-JaVerT-switch-01      0 gillian-js verify --certified-smt --dump-smt $JS/JaVerT/test262/switch-01.js
case_ 09-js-JaVerT-switch-02      0 gillian-js verify --certified-smt --dump-smt $JS/JaVerT/test262/switch-02.js
case_ 10-js-JaVerT-try-catch-01   1 gillian-js verify --certified-smt --dump-smt $JS/JaVerT/test262/try-catch-01.js
case_ 11-js-JaVerT-try-catch-02   0 gillian-js verify --certified-smt --dump-smt $JS/JaVerT/test262/try-catch-02.js
case_ 12-js-JaVerT-try-catch-03   0 gillian-js verify --certified-smt --dump-smt $JS/JaVerT/test262/try-catch-03.js
case_ 13-js-Amazon-main       '0|124' gillian-js verify $JS/Amazon/deserialize_factory.js --no-lemma-proof -l disabled --certified-smt
case_ 14-js-Amazon-pp-bug       125 gillian-js verify $JS/Amazon/bugs/pp/deserialize_factory.js --no-lemma-proof -l normal --certified-smt
case_ 15-js-Amazon-frozen-bug   125 gillian-js verify $JS/Amazon/bugs/frozen/deserialize_factory.js --no-lemma-proof --certified-smt

case_ 16-c-verification-dll       0 gillian-c verify --certified-smt $C/verification/dll.c
case_ 17-c-verification-priQ      0 gillian-c verify --certified-smt $C/verification/priQ.c
case_ 18-c-verification-sll       0 gillian-c verify --certified-smt $C/verification/sll.c
case_ 19-c-verification-sort      0 gillian-c verify --certified-smt $C/verification/sort.c
case_ 20-c-verification-vector    0 gillian-c verify --certified-smt $C/verification/vector.c
case_ 21-c-Amazon-main        '0|124' gillian-c verify "${AMZ_C[@]}" --fstruct-passing --no-lemma-proof -l disabled --certified-smt
case_ 22-c-Amazon-byte-cursor-ub  1 gillian-c verify $C/amazon/bugs/byte_buf.c $C/amazon/allocator.c $C/amazon/error.c $C/amazon/base.c --fstruct-passing --no-lemma-proof --proc aws_byte_cursor_advance -l disabled --certified-smt

case_ 23-wisl-DLL_recursive       0 wisl verify --certified-smt ./wisl/examples/DLL_recursive.wisl
case_ 24-wisl-SLL_iterative       0 wisl verify --certified-smt ./wisl/examples/SLL_iterative.wisl
case_ 25-wisl-SLL_recursive       0 wisl verify --certified-smt ./wisl/examples/SLL_recursive.wisl
case_ 26-wisl-concurrent_binary_tree 0 wisl verify --certified-smt ./wisl/examples/frac/concurrent_binary_tree.wisl
case_ 27-wisl-floating_point      0 wisl verify --certified-smt ./wisl/examples/frac/floating_point.wisl
case_ 28-wisl-lambda_terms        1 wisl verify --certified-smt ./wisl/examples/frac/lambda_terms.wisl
case_ 29-wisl-simple_concurrency  1 wisl verify --certified-smt ./wisl/examples/frac/simple_concurrency.wisl
case_ 30-wisl-wildcard            1 wisl verify --certified-smt ./wisl/examples/frac/wildcard.wisl
case_ 31-wisl-sll_cc              1 wisl verify --certified-smt ./wisl/examples/sll_cc.wisl

echo "==> $(wc -l < "$OUT/queries.jsonl" | tr -d ' ') queries in $OUT/queries.jsonl"
if [ "$UNEXPECTED" -ne 0 ]; then
  echo "==> some cases exited unexpectedly; see $OUT/status.tsv and $OUT/logs/" >&2
fi
exit "$UNEXPECTED"
