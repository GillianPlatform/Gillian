#!/bin/bash

set -e

if [[ -z "${GITHUB_ACTIONS}" ]]; then
	echo "Building test environment..."
	esy x true > /dev/null 2>&1
	esy exec-env > exec.env
	source exec.env
fi
# Bash array format: ("one" "two" "three")
# JS Files to test
declare -a jsfiles=("BST" "PriQ" "SLL" "DLL" "Sort")

FINAL_RETURN=0

if [[ $1 == "fast" ]]; then
	params="-nochecks -nooutput"
else
	params=""
fi

foldername=${PWD##*/}
if [ "$foldername" != "environment" ]
then
	cd environment
fi

echo "Verifying JaVerT examples"
echo "-------------------------"
for f in "${jsfiles[@]}"
do
  sleep 1
	echo "Verifying: $f.js"
	if [[ $1 == "count" ]]; then
		gillian-js verify Examples/JaVerT/$f.js -l disabled --stats --no-lemma-proof
		rc=$?
	else
		gillian-js verify Examples/JaVerT/$f.js -l disabled --no-lemma-proof
		rc=$?
	fi
	if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
	echo "----------------"
done
exit $FINAL_RETURN