#!/bin/bash

set -e

WPST="time opam exec -- gillian-c wpst"

# TODO (Alexis): Make incremental analysis thread-safe to allow the use of --parallel
echo "--- testing SLL ---"
$WPST symbolic/sll.c -l disabled
printf "\n\n"

echo "--- testing DLL ---"
$WPST symbolic/dll.c -l disabled
printf "\n\n"

echo "--- testing PriQ ---"
$WPST symbolic/priQ.c -l disabled
printf "\n\n"

echo "--- testing kvmap ---"
$WPST symbolic/kvmap.c -l disabled
printf "\n\n"

echo "--- testing sort ---"
$WPST symbolic/sort.c -l disabled
printf "\n\n"

echo "--- testing BST ---"
$WPST symbolic/bst.c -l disabled
printf "\n\n"
