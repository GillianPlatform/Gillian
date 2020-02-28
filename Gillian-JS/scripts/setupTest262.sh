#!/bin/bash

# Create test breakdown
cd test262/test/breakdown
./createBreakdown.sh
cd ../../../

# Copy tests and breakdown
rm -rf environment/Test262
mkdir -p environment/Test262/breakdown
mkdir -p environment/Test262/harness
mkdir -p environment/Test262/test
cp test262/test/breakdown/*.tests environment/Test262/breakdown
cp -r test262/harness environment/Test262
cp -r test262/test/built-ins environment/Test262/test
cp -r test262/test/language environment/Test262/test
