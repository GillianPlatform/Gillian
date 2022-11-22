#!/bin/sh

echo "===== BUILDING ODOC =====" && \
esy odoc && \
echo && \
echo "===== BUILDING SPHINX =====" && \
esy sphinx
