#!/usr/bin/env bash

if [[ "${GITHUB_ACTIONS}" ]]; then
	GILLIAN_C="gillian-c"
else
  GILLIAN_C="dune exec -- gillian-c"
fi

VERIFY="time $GILLIAN_C verify"

echo "--- verifying SLL ---"
$VERIFY verification/sll.c -l disabled
printf "\n\n"

echo "--- verifying DLL ---"
$VERIFY verification/dll.c -l disabled
printf "\n\n"

echo "--- verifying PriQ ---"
$VERIFY verification/priQ.c -l disabled
printf "\n\n"

echo "--- verifying sort ---"
$VERIFY verification/sort.c -l disabled
printf "\n\n"

echo "--- verifying vector ---"
$VERIFY verification/vector.c -l disabled
printf "\n\n"

echo "--- verifying array ---"
$VERIFY verification/array.c -l disabled
printf "\n\n"

echo "--- verifying batch ---"
$VERIFY verification/batch.c -l disabled
printf "\n\n"
