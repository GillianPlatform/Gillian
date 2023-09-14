MODE=$1
FILE=$2
cbmc $FILE --show-symbol-table --json-ui | jq '.[] | select(has("symbolTable"))' > $FILE.symtab.json
esy x kanillian $MODE $FILE.symtab.json -o ${FILE%".c"}.gil --json-ui