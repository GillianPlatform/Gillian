FINAL_RETURN=0

esy x true > /dev/null 2>&1
esy exec-env > exec.env
source exec.env

verify () {
  echo "\nVerifying: $1\n"
  wisl verify $1
  rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
}

verify_exv () {
  echo "\nVerifying: $1\n"
  wisl verify --exv $1
  rc=$?; if [[ $rc != 0 ]]; then FINAL_RETURN=1; fi
}

echo "--- WISL: OX VERIFICATION ---"
verify wisl/examples/SLL_recursive.wisl
verify wisl/examples/DLL_recursive.wisl

echo "\n\n--- WISL: EXACT VERIFICATION ---"
verify_exv wisl/examples/SLL_ex_ongoing.wisl

exit $FINAL_RETURN