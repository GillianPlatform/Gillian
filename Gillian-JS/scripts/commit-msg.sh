#!/bin/sh

# Colors are beautiful
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "${BLUE}info:${NC} Executing commit-msg hook to check your commit message"

# Does commitlint exist ?
COMMITLINT=$(esy which commitlint)
if [ "$COMMITLINT" = "" ];
then
    # Commitlint does not exist :(
    echo "${RED}err:${NC}  Commitlint is not installed, try : "
    echo "             ${BLUE}esy install${NC}"
    exit 1
fi

# Commitlint is installed
cat $1 | esy commitlint
if [ $? == 1 ]; then
    # Commitlint returned 1, scream.
    echo "\n\n${RED}err:${NC}  Please correct your commit message !"
    exit 1
fi

# Do not forget to be nice to people when they do things well :D
echo "\n\n${GREEN}success:${NC} Thank you for writing a nice commit message !"
