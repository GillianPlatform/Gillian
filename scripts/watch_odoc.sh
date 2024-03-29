#!/bin/bash

if [ -x "$(command -v inotifywait)" ]; then
  echo "Watching with inotifywait"
  inotifywait -qmr -e modify,create,delete ./_build/default/_doc/_html | xargs -n1 -I{} "./scripts/sync_odoc.sh"
elif [ -x "$(command -v fswatch)" ]; then
  echo "Watching with fswatch"
  fswatch -o ./_build/default/_doc/_html | xargs -n1 -I{} "./scripts/sync_odoc.sh"
else
  echo "Couldn't find inotifywait or fswatch!" >&2
fi
