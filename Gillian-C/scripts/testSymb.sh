#!/usr/bin/env bash

set -e

if [[ "${GITHUB_ACTIONS}" ]]; then
	GILLIAN_C="gillian-c"
else
  GILLIAN_C="dune exec -- gillian-c"
fi

WPST="time $GILLIAN_C wpst"

# TODO (Alexis): Make incremental analysis thread-safe to allow the use of --parallel
echo "--- testing SLL ---"
$WPST symbolic/sll.c -l disabled
printf "\n\n"

echo "--- testing DLL ---"
$WPST symbolic/dll.c -l disabled
printf "\n\n"

echo "--- testing PriQ ---"
$WPST symbolic/priQ.c -l disabled
printf "\n\n"

echo "--- testing kvmap ---"
$WPST symbolic/kvmap.c -l disabled
printf "\n\n"

echo "--- testing sort ---"
$WPST symbolic/sort.c -l disabled
printf "\n\n"

echo "--- testing BST ---"
$WPST symbolic/bst.c -l disabled
printf "\n\n"

echo "--- testing globalvar ---"
$WPST symbolic/globalvar.c -l disabled
printf "\n\n"

echo "--- testing unstructured ---"
$WPST symbolic/unstructured.c -l disabled
printf "\n\n"

# Expected-failure test: array_oob.c contains an out-of-bounds bug that
# symbolic testing must catch, so a zero exit code here is itself a failure.
echo "--- testing array_oob (expect failure: out-of-bounds bug) ---"
if $WPST symbolic/bug_examples/array_oob.c -l disabled; then
  echo "ERROR: expected array_oob.c to fail, but it succeeded"
  exit 1
else
  echo "(failed as expected)"
fi
printf "\n\n"
