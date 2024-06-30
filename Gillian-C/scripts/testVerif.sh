#!/bin/bash

VERIFY="time opam exec -- gillian-c verify"

echo "--- verifying SLL ---"
$VERIFY verification/sll.c -l disabled
printf "\n\n"

echo "--- verifying DLL ---"
$VERIFY verification/dll.c -l disabled
printf "\n\n"

echo "--- verifying PriQ ---"
$VERIFY verification/priQ.c -l disabled
printf "\n\n"

echo "--- verifying sort ---"
$VERIFY verification/sort.c -l disabled
printf "\n\n"