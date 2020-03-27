#!/bin/bash

FINAL_RETURN=0

if [[ -z "${GITHUB_ACTIONS}" ]]; then
  esy x true > /dev/null 2>&1
  esy exec-env > exec.env
  source exec.env
fi

echo "--- executing test ---"
gillian-c exec with-imports/concrete/test.c with-imports/concrete/foo.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- verifying BST ---"
gillian-c verify with-imports/verification/bst.c with-imports/verification/bst_node.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- bi-abducing BST ---"
gillian-c act with-imports/act/bst.c with-imports/act/bst_node.c -l disabled
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"
