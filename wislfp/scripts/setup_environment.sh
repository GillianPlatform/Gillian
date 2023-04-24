#!/bin/bash

foldername=${PWD##*/}
if [ "$foldername" != "wisl" ]
then
	cd wisl
fi

mkdir -p environment
cp -r Examples/* environment/
cp scripts/*.sh environment