#!/usr/bin/env bash

FINAL_RETURN=0

echo "--- verifying SLL ---"
time gillian-c2 verify verification/sll.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- verifying DLL ---"
time gillian-c2 verify verification/dll.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

exit $FINAL_RETURN
