#!/bin/bash
foldername=${PWD##*/}
if [ "$foldername" != "kanillian" ]
then
	cd kanillian
fi
mkdir -p environment

cp scripts/*.sh environment
cp -R examples/* environment
