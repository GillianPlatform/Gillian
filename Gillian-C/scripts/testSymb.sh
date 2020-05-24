#!/bin/bash

FINAL_RETURN=0

if [[ -z "${GITHUB_ACTIONS}" ]]; then
  esy x true > /dev/null 2>&1
  esy exec-env > exec.env
  source exec.env
fi

# TODO (Alexis): Make incremental analysis thread-safe to allow the use of --parallel
echo "--- testing SLL ---"
time gillian-c wpst symbolic/sll.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- testing DLL ---"
time gillian-c wpst symbolic/dll.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- testing PriQ ---"
time gillian-c wpst symbolic/priQ.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- testing kvmap ---"
time gillian-c wpst symbolic/kvmap.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- testing sort ---"
time gillian-c wpst symbolic/sort.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- testing BST ---"
time gillian-c wpst symbolic/bst.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

exit $FINAL_RETURN