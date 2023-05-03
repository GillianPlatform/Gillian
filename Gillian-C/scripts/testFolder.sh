#!/bin/bash

folder=$1
count=$2

if [[ -z "${GITHUB_ACTIONS}" ]]; then
    esy x true > /dev/null 2>&1
    esy exec-env > exec.env
    source exec.env
fi

for filename in $folder/*.c; do
    [ -f "$filename" ] || break
    echo $filename
    if [ "$count" = "count" ]; then
        time gillian-c wpst "$filename" -l disabled --stats
    else
        time gillian-c wpst "$filename" -l disabled
    fi
done