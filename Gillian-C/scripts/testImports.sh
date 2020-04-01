#!/bin/bash

FINAL_RETURN=0

if [[ -z "${GITHUB_ACTIONS}" ]]; then
  esy x true > /dev/null 2>&1
  esy exec-env > exec.env
  source exec.env
fi

echo "--- executing test.c ---"
gillian-c exec with-imports/concrete/test.c with-imports/concrete/foo.c -s
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"
