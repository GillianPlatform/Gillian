#!/bin/bash

cmds=$2

if [ "$cmds" = "count" ]; then
    ./testCosetteFolder.sh Examples/Cosette/Buckets/$1 count
else
    ./testCosetteFolder.sh Examples/Cosette/Buckets/$1
fi
sleep 1
