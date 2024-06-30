rm *.log
rm *.bc
rm -r klee-out-* klee-last
cd ..
if [[ $1 == '--nomake' ]]
then
  echo 'Not recompiling, only resetting env'
else
  opam exec -- dune build
fi

cd environment