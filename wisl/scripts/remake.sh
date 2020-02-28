cd ..
if [[ $1 == '--nomake' ]]
then
  echo 'Not recompiling, only resetting env'
else
  esy
fi
esy init:env
cd environment