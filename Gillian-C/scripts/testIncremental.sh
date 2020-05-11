#!/bin/bash

set -e

FINAL_RETURN=0

if [[ -z "${GITHUB_ACTIONS}" ]]; then
  esy x true > /dev/null 2>&1
  esy exec-env > exec.env
  source exec.env
fi

echo "--- test_add_proc ---"
incremental/verification/test_add_proc/run.sh
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- test_change_header ---"
incremental/verification/test_change_header/run.sh
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- test_change_proc_body ---"
incremental/verification/test_change_proc_body/run.sh
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- test_remove_proc ---"
incremental/verification/test_remove_proc/run.sh
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"

echo "--- test_change_pred ---"
incremental/verification/test_remove_proc/run.sh
rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
printf "\n\n"
