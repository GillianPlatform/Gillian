#!/bin/bash

if [[ -z "${GITHUB_ACTIONS}" ]]; then
  esy x true > /dev/null 2>&1
  esy exec-env > exec.env
  source exec.env
fi

filename=$1

echo $filename

time gillian-js wpst "$filename" -l disabled --stats