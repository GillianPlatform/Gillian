#!/usr/bin/env bash

folder=$1

for filename in $folder/*.js; do
    [ -f "$filename" ] || break
    ./testCosette.sh $filename
done