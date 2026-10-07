#!/usr/bin/env bash
set -euo pipefail

ROOT="${GILLIAN_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
TIMEOUT_SECONDS="${CERTIFIED_SMT_TIMEOUT_SECONDS:-120}"
cd "$ROOT"

# Build up front, so that no case's timeout is spent building.
dune build

# The verifier appends to the experiment log, so a rerun that did not start
# from an empty file would mix this corpus with whatever ran before it. Clear
# the log and the per-case output here, so the artefacts in this directory
# always describe exactly one run of this script.
mkdir -p experiments/certified-smt-rerun-logs
rm -f experiments/certified-smt-rerun-logs/*.out experiments/certified-smt-rerun-logs/*.err
: > experiments/certified-smt.jsonl
FAILED=0

run_cmd() {
  local id="$1"
  shift
  printf "[%s]" "$id"
  for arg in "$@"; do printf " %q" "$arg"; done
  printf "\n"
  set +e
  timeout "$TIMEOUT_SECONDS" "$@" >"experiments/certified-smt-rerun-logs/${id}.out" 2>"experiments/certified-smt-rerun-logs/${id}.err"
  local status=$?
  set -e
  printf "%s %s\n" "$id" "$status" | tee -a experiments/certified-smt-rerun-logs/status.tsv >/dev/null
  if [ "$status" -ne 0 ]; then
    FAILED=1
  fi
}

: > experiments/certified-smt-rerun-logs/status.tsv

run_cmd '00-js-JaVerT-BST' dune exec -- gillian-js verify --certified-smt --dump-smt ./Gillian-JS/Examples/JaVerT/BST.js
run_cmd '01-js-JaVerT-DLL' dune exec -- gillian-js verify --certified-smt --dump-smt ./Gillian-JS/Examples/JaVerT/DLL.js
run_cmd '02-js-JaVerT-ExprEval' dune exec -- gillian-js verify --certified-smt --dump-smt ./Gillian-JS/Examples/JaVerT/ExprEval.js
run_cmd '03-js-JaVerT-IDGen' dune exec -- gillian-js verify --certified-smt --dump-smt ./Gillian-JS/Examples/JaVerT/IDGen.js
run_cmd '04-js-JaVerT-PriQ' dune exec -- gillian-js verify --certified-smt --dump-smt ./Gillian-JS/Examples/JaVerT/PriQ.js
run_cmd '05-js-JaVerT-SLL' dune exec -- gillian-js verify --certified-smt --dump-smt ./Gillian-JS/Examples/JaVerT/SLL.js
run_cmd '06-js-JaVerT-Sort' dune exec -- gillian-js verify --certified-smt --dump-smt ./Gillian-JS/Examples/JaVerT/Sort.js
run_cmd '07-js-JaVerT-annotated-SLL' dune exec -- gillian-js verify --certified-smt --dump-smt ./Gillian-JS/Examples/JaVerT/annotated/SLL.js
run_cmd '08-js-JaVerT-switch-01' dune exec -- gillian-js verify --certified-smt --dump-smt ./Gillian-JS/Examples/JaVerT/test262/switch-01.js
run_cmd '09-js-JaVerT-switch-02' dune exec -- gillian-js verify --certified-smt --dump-smt ./Gillian-JS/Examples/JaVerT/test262/switch-02.js
run_cmd '10-js-JaVerT-try-catch-01' dune exec -- gillian-js verify --certified-smt --dump-smt ./Gillian-JS/Examples/JaVerT/test262/try-catch-01.js
run_cmd '11-js-JaVerT-try-catch-02' dune exec -- gillian-js verify --certified-smt --dump-smt ./Gillian-JS/Examples/JaVerT/test262/try-catch-02.js
run_cmd '12-js-JaVerT-try-catch-03' dune exec -- gillian-js verify --certified-smt --dump-smt ./Gillian-JS/Examples/JaVerT/test262/try-catch-03.js
run_cmd '13-js-Amazon-main' dune exec -- gillian-js verify ./Gillian-JS/Examples/Amazon/deserialize_factory.js --no-lemma-proof -l disabled --certified-smt
run_cmd '14-js-Amazon-pp-bug' dune exec -- gillian-js verify ./Gillian-JS/Examples/Amazon/bugs/pp/deserialize_factory.js --no-lemma-proof -l normal --certified-smt
run_cmd '15-js-Amazon-frozen-bug' dune exec -- gillian-js verify ./Gillian-JS/Examples/Amazon/bugs/frozen/deserialize_factory.js --no-lemma-proof --certified-smt

run_cmd '16-c-verification-dll' dune exec -- gillian-c verify --certified-smt ./Gillian-C/examples/verification/dll.c
run_cmd '17-c-verification-priQ' dune exec -- gillian-c verify --certified-smt ./Gillian-C/examples/verification/priQ.c
run_cmd '18-c-verification-sll' dune exec -- gillian-c verify --certified-smt ./Gillian-C/examples/verification/sll.c
run_cmd '19-c-verification-sort' dune exec -- gillian-c verify --certified-smt ./Gillian-C/examples/verification/sort.c
run_cmd '20-c-verification-vector' dune exec -- gillian-c verify --certified-smt ./Gillian-C/examples/verification/vector.c
run_cmd '21-c-Amazon-main' dune exec -- gillian-c verify ./Gillian-C/examples/amazon/header.c ./Gillian-C/examples/amazon/edk.c ./Gillian-C/examples/amazon/array_list.c ./Gillian-C/examples/amazon/ec.c ./Gillian-C/examples/amazon/byte_buf.c ./Gillian-C/examples/amazon/hash_table.c ./Gillian-C/examples/amazon/string.c ./Gillian-C/examples/amazon/allocator.c ./Gillian-C/examples/amazon/error.c ./Gillian-C/examples/amazon/base.c --fstruct-passing --no-lemma-proof -l disabled --certified-smt
run_cmd '22-c-Amazon-byte-cursor-ub' dune exec -- gillian-c verify ./Gillian-C/examples/amazon/bugs/byte_buf.c ./Gillian-C/examples/amazon/allocator.c ./Gillian-C/examples/amazon/error.c ./Gillian-C/examples/amazon/base.c --fstruct-passing --no-lemma-proof --proc aws_byte_cursor_advance -l disabled --certified-smt

run_cmd '23-wisl-DLL_recursive' dune exec -- wisl verify --certified-smt ./wisl/examples/DLL_recursive.wisl
run_cmd '24-wisl-SLL_iterative' dune exec -- wisl verify --certified-smt ./wisl/examples/SLL_iterative.wisl
run_cmd '25-wisl-SLL_recursive' dune exec -- wisl verify --certified-smt ./wisl/examples/SLL_recursive.wisl
run_cmd '26-wisl-concurrent_binary_tree' dune exec -- wisl verify --certified-smt ./wisl/examples/frac/concurrent_binary_tree.wisl
run_cmd '27-wisl-floating_point' dune exec -- wisl verify --certified-smt ./wisl/examples/frac/floating_point.wisl
run_cmd '28-wisl-lambda_terms' dune exec -- wisl verify --certified-smt ./wisl/examples/frac/lambda_terms.wisl
run_cmd '29-wisl-simple_concurrency' dune exec -- wisl verify --certified-smt ./wisl/examples/frac/simple_concurrency.wisl
run_cmd '30-wisl-wildcard' dune exec -- wisl verify --certified-smt ./wisl/examples/frac/wildcard.wisl
run_cmd '31-wisl-sll_cc' dune exec -- wisl verify --certified-smt ./wisl/examples/sll_cc.wisl

exit "$FAILED"
