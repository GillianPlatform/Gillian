rm -r doc
opam exec -- dune build @doc
cp -r _build/default/_doc/_html doc