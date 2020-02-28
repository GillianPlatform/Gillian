#!/bin/bash

file=$1
fold=${file%/*}
name=${file##*/} 

filename=$(echo "$file" | cut -f 1 -d '.')

RED='\033[0;31m'
LIGHTGREEN='\033[1;32m'
NC='\033[0m'

time {
	echo ""
	echo "-------- Verifying: $file --------"
	il=il;
	bi=BI_;
	echo "Running JS-2-JSIL on file $file"
	./js2jsil.native -file $file -bi &> /dev/null  
	rc=$?; if [[ $rc != 0 ]]; then printf "${RED}Failed JS-2-JSIL on $file ${NC}\n"; fi
	echo "Running ACT on file $file"
	./act.native -file $filename.gil -js -silent > $file.specs
	rc=$?; if [[ $rc != 0 ]]; then echo "Failed ACT on $file"; fi
	printf "${LIGHTGREEN}" 
	tail -3 $file.specs 
	printf "${NC}"
}
#echo ""
#echo "Verifying bi-abduced specifications for file $file"
#./jsilverify.native -file $fold/$bi$name$il -silent
sleep 5

