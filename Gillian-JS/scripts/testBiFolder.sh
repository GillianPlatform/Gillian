#!/bin/bash

folder=$1
count=$2

for filename in $folder*.js; do
    [ -f "$filename" ] || break
    if [ "$count" = "count" ]; then
        ./testBiFile.sh $filename count
    else
        ./testBiFile.sh $filename
    fi
done
