#!/bin/bash

FINAL_RETURN=0

if [[ -z "${GITHUB_ACTIONS}" ]]; then
  esy x true > /dev/null 2>&1
  esy exec-env > exec.env
  source exec.env
fi

# echo "--- executing main.c ---"
# gillian-c exec -l disabled multifile/concrete/main.c multifile/concrete/foo.c
# rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
# printf "\n\n"

echo "--- verifying BST ---"
gillian-c verify -l disabled multifile/verification/sll_a.c multifile/verification/sll_b.c
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

# echo "--- bi-abducing BST ---"
# gillian-c act -l disabled multifile/act/bst_a.c multifile/act/bst_b.c
# rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
# printf "\n\n"
