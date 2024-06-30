#!/bin/bash

folder=$1
count=$2

WPST="opam exec -- gillian-c wpst"

for filename in $folder/*.c; do
    [ -f "$filename" ] || break
    echo $filename
    if [ "$count" = "count" ]; then
        time $WPST "$filename" -l disabled --stats
    else
        time $WPST "$filename" -l disabled
    fi
done