#!/usr/bin/env bash

filename=$1

echo $filename

time gillian-js wpst "$filename" -l disabled --stats
