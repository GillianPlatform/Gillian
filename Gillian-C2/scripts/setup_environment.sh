#!/bin/bash
foldername=${PWD##*/}
if [ "$foldername" != "Gillian-C2" ]
then
	cd Gillian-C2
fi
mkdir -p environment

cp scripts/*.sh environment
cp -R examples/* environment
