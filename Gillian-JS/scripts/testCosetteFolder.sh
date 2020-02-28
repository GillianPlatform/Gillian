#!/bin/bash

folder=$1
count=$2

for filename in $folder/*.js; do
    [ -f "$filename" ] || break
    if [ "$count" = "count" ]; then
        ./testCosette.sh $filename
    else
        ./testCosetteParallel.sh $filename
    fi
done