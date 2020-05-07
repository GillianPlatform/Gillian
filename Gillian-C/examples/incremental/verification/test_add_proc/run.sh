#!/usr/bin/env bash

set -e

SCRIPT_DIR="$(dirname "$(realpath "$0")")"
cd ${SCRIPT_DIR}

./clean.sh

SRC_BEFORE_DIR="src_before"
SRC_AFTER_DIR="src_after"
SRC_FILES=("bst_a.c" "bst_b.c")
EXPECTED_DIFF="expected_diff.txt"
ACTUAL_DIFF=".gillian/diff.txt"

echo "Analysing files in ${SRC_BEFORE_DIR}..."
cp ${SRC_BEFORE_DIR}/* .
gillian-c verify -l disabled --inc ${SRC_FILES[@]}

echo "Analysing files in in ${SRC_AFTER_DIR}..."
cp ${SRC_AFTER_DIR}/* .
gillian-c verify -l disabled --inc ${SRC_FILES[@]}

RED_COLOUR=$(tput setaf 1)
GREEN_COLOUR=$(tput setaf 2)
RESET_COLOURS=$(tput sgr0)

echo "Comparing changes..."
if diff --brief ${EXPECTED_DIFF} ${ACTUAL_DIFF}; then
  echo "${GREEN_COLOUR}PASS${RESET_COLOURS}"
  ./clean.sh
  exit 0
else
  echo "${RED_COLOUR}FAIL${RESET_COLOURS}"
  diff --side-by-side ${EXPECTED_DIFF} ${ACTUAL_DIFF}
  exit 1
fi
