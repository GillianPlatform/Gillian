#!/usr/bin/env bash

set -e

if [[ "${GITHUB_ACTIONS}" ]]; then
	VERIFY="gillian-js verify"
	WPST="gillian-js wpst"
else
  VERIFY="dune exec -- gillian-js verify"
  WPST="dune exec -- gillian-js wpst"
fi

# Bash array format: ("one" "two" "three")
# JaVerT verification examples to test
declare -a jsfiles=("BST" "PriQ" "SLL" "DLL" "Sort" "ExprEval" "IDGen" "KVMap" "annotated/SLL")

# Cosette symbolic-testing (wpst) case studies, relative to Examples/
declare -a wpstfiles=(
	"Cosette/CaseStudies/BST/bst_find_1"
	"Cosette/CaseStudies/BST/bst_find_min_1"
	"Cosette/CaseStudies/BST/bst_insert_1"
	"Cosette/CaseStudies/BST/bst_remove_1"
	"Cosette/CaseStudies/DLL/dll"
	"Cosette/CaseStudies/ExprEval/exprEval_3"
	"Cosette/CaseStudies/ExprEval/exprEval_4"
	"Cosette/CaseStudies/ExprEval/exprEval_5"
	"Cosette/CaseStudies/ExprEval/exprEval_6"
	"Cosette/CaseStudies/ExprEval/exprEval_7"
	"Cosette/CaseStudies/IDGen/IdGen"
	"Cosette/CaseStudies/KVMap/kv_map_1"
	"Cosette/CaseStudies/KVMap/kv_map_2"
	"Cosette/CaseStudies/PriQ/pri_q_1"
	"Cosette/CaseStudies/PriQ/pri_q_2"
	"Cosette/CaseStudies/SLL/sll"
	"Cosette/CaseStudies/Sort/sort_1"
	"Cosette/CaseStudies/Sort/sort_2"
	"Cosette/simple_example"
	"Cosette/check_div_by_zero"
)

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

echo "Symbolic-testing (wpst) Cosette case studies"
echo "--------------------------------------------"
for f in "${wpstfiles[@]}"
do
  sleep 1
	echo "Testing: $f.js"
	if [[ $1 == "count" ]]; then
		$WPST Examples/$f.js -l disabled --stats
		rc=$?
	else
		$WPST Examples/$f.js -l disabled
		rc=$?
	fi
	if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
	echo "----------------"
done
exit $FINAL_RETURN
