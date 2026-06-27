set -e

verify () {
  echo "\nVerifying: $1\n"
  wisl verify $1 -l disabled
}

echo "--- WISL: OX VERIFICATION ---"
verify wisl/examples/SLL_recursive.wisl
verify wisl/examples/SLL_iterative.wisl
verify wisl/examples/DLL_recursive.wisl

# echo "\n\n--- WISL: EXACT VERIFICATION ---"
# verify_exv wisl/examples/SLL_ex_ongoing.wisl

exit $FINAL_RETURN