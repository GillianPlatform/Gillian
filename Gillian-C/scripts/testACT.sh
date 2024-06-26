#!/bin/bash

set -e

ACT="gillian-c act"

echo "--- bi-abducing SLL ---"
time $ACT act/sll.c -l disabled
printf "\n\n"

echo "--- bi-abducing DLL ---"
time $ACT act/dll.c -l disabled
printf "\n\n"

echo "--- bi-abducing PriQ ---"
time $ACT act/priQ.c -l disabled
printf "\n\n"

echo "--- bi-abducing kvmap ---"
time $ACT act/kvmap.c -l disabled
printf "\n\n"

echo "--- bi-abducing sort ---"
time $ACT act/sort.c -l disabled
printf "\n\n"

echo "--- bi-abducing BST ---"
time $ACT act/bst.c -l disabled
printf "\n\n"