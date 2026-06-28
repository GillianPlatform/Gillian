#!/usr/bin/env bash

FINAL_RETURN=0

if [[ "${GITHUB_ACTIONS}" ]]; then
	GILLIAN_C2="gillian-c2"
else
  GILLIAN_C2="dune exec -- gillian-c2"
fi
VERIFY="time $GILLIAN_C2 verify -l disabled"

echo "--- verifying SLL ---"
$VERIFY verification/sll.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- verifying DLL ---"
$VERIFY verification/dll.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- verifying CC SLL ---"
$VERIFY verification/cc_slist.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

exit $FINAL_RETURN
