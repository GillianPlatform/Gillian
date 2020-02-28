#!/bin/bash
foldername=${PWD##*/}
if [ "$foldername" != "Gillian-JS" ]
then
	cd Gillian-JS
fi
mkdir -p environment
cp -r Examples environment

cp scripts/*.sh environment
