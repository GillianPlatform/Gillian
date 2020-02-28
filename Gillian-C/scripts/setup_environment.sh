#!/bin/bash
foldername=${PWD##*/}
if [ "$foldername" != "Gillian-C" ]
then
	cd Gillian-C
fi
mkdir -p environment

cp scripts/*.sh environment
cp -R examples/* environment