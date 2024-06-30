MODE=$1
FILE=$2
cbmc $FILE --show-symbol-table --json-ui > $FILE.symtab.json
opam exec -- kanillian $MODE $FILE.symtab.json -o ${FILE%".c"}.gil --json-ui