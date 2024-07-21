#!/bin/bash
trap terminate SIGINT
terminate(){
    pkill -SIGINT -P $$
    exit
}
make odoc-watch | xargs -I{} echo '[ odoc ] {}' &
make sphinx-watch | xargs -I{} echo '[sphinx] {}' &
wait