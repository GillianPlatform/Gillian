#!/bin/bash
inotifywait -qmr -e modify,create,delete ./_build/default/_doc/_html | xargs -n1 -I{} "./scripts/sync_odoc.sh"
# fswatch -o ./_build/default/_doc/_html | xargs -n1 -I{} "./scripts/sync_odoc.sh"
