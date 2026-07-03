set -e

verify () {
  echo "\nVerifying: $1\n"
  wislf verify $1 -l disabled
}

echo "--- WISL: OX VERIFICATION ---"
verify wisl/examples/frac/simple_concurrency.wisl
verify wisl/examples/frac/concurrent_binary_tree.wisl
verify wisl/examples/frac/floating_point.wisl
verify wisl/examples/frac/lambda_terms.wisl

# echo "\n\n--- WISL: EXACT VERIFICATION ---"
# verify_exv wisl/examples/SLL_ex_ongoing.wisl

exit $FINAL_RETURN
