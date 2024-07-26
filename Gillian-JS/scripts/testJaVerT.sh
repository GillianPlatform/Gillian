#!/bin/bash

set -e

if [[ "${GITHUB_ACTIONS}" ]]; then
	VERIFY="opam exec -- gillian-js verify"
else
  VERIFY="dune exec -- gillian-js verify"
fi

# Bash array format: ("one" "two" "three")
# JS Files to test
declare -a jsfiles=("BST" "PriQ" "SLL" "DLL" "Sort")

FINAL_RETURN=0

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
		$VERIFY Examples/JaVerT/$f.js -l disabled --stats --no-lemma-proof
		rc=$?
	else
		$VERIFY Examples/JaVerT/$f.js -l disabled --no-lemma-proof
		rc=$?
	fi
	if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
	echo "----------------"
done
exit $FINAL_RETURN