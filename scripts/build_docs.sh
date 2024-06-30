#!/bin/sh

echo "===== BUILDING ODOC =====" && \
 odoc && \
echo && \
echo "===== BUILDING SPHINX =====" && \
esy sphinx
