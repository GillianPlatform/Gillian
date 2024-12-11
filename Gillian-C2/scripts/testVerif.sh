#!/bin/bash

FINAL_RETURN=0

if [[ -z "${GITHUB_ACTIONS}" ]]; then
  esy x true > /dev/null 2>&1
  esy exec-env > exec.env
  source exec.env
fi

echo "--- verifying SLL ---"
time gillian-c2 verify verification/list_unbounded.gil -a
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- verifying DLL ---"
time gillian-c2 verify verification/list_std.gil -a
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

exit $FINAL_RETURN
