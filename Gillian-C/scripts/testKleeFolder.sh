#!/bin/bash

folder=$1
rm -r $folder/klee-out-*

time {
for filename in $folder/*.c; do
    [ -f "$filename" ] || break
    echo ""
    echo "GOING TO ANALYSE : $filename"
    echo ""
    echo "" >> klee_output.log
    echo "GOING TO ANALYSE : $filename" >> klee_output.log
    time ((clang -I $KLEE_INCLUDE -emit-llvm -c -g -O0 -Xclang -disable-O0-optnone $filename > /dev/null 2>&1) && (klee -libc=klee $(basename $filename .c).bc >> klee_output.log 2>&1))
done
}