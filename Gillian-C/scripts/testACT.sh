#!/bin/bash

FINAL_RETURN=0
if [[ -z "${GITHUB_ACTIONS}" ]]; then
  esy x true > /dev/null 2>&1
  esy exec-env > exec.env
  source exec.env
fi

echo "--- bi-abducing SLL ---"
time gillian-c act act/sll.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- bi-abducing DLL ---"
time gillian-c act act/dll.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- bi-abducing PriQ ---"
time gillian-c act act/priQ.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- bi-abducing kvmap ---"
time gillian-c act act/kvmap.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- bi-abducing sort ---"
time gillian-c act act/sort.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- bi-abducing BST ---"
time gillian-c act act/bst.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

exit $FINAL_RETURN