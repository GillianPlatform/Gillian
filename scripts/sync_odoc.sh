#!/bin/sh

mkdir -p _docs
rsync -auv --delete _build/default/_doc/_html/. _docs/odoc/
