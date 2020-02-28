rm -r doc
esy dune build @doc
cp -r _build/default/_doc/_html doc