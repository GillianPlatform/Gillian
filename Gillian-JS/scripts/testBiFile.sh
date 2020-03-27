#!/bin/bash

file=$1
fold=${file%/*}
name=${file##*/}

RED='\033[0;31m'
LIGHTGREEN='\033[1;32m'
NC='\033[0m'

time {
	echo ""
	echo "-------- Verifying: $file --------"
	il=il;
	bi=BI_;
	echo "Running ACT on file $file"
	esy x gillian-js act $file -l disabled > $file.specs
	rc=$?; if [[ $rc != 0 ]]; then echo "Failed ACT on $file"; fi
	printf "${LIGHTGREEN}"
	tail -3 $file.specs
	printf "${NC}"
}
#echo ""
#echo "Verifying bi-abduced specifications for file $file"
#./jsilverify.native -file $fold/$bi$name$il -silent
sleep 5

