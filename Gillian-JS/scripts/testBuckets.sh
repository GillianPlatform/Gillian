#!/bin/bash

foldername=${PWD##*/}
if [ "$foldername" != "environment" ]
then
	cd environment
fi

./testBucketsFolder.sh arrays
sleep 5
./testBucketsFolder.sh bag
sleep 5
./testBucketsFolder.sh bstree
sleep 5
./testBucketsFolder.sh dictionary
sleep 5
./testBucketsFolder.sh heap
sleep 5
./testBucketsFolder.sh linkedlist
./testBucketsFolder.sh linkedlist/bug
sleep 5
./testBucketsFolder.sh multidictionary
./testBucketsFolder.sh multidictionary/bug
sleep 5
./testBucketsFolder.sh queue
sleep 5
./testBucketsFolder.sh priorityqueue
sleep 5
./testBucketsFolder.sh set
sleep 5
./testBucketsFolder.sh stack