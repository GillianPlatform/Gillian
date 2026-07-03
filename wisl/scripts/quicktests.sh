set -e

verify () {
  echo "\nVerifying: $1\n"
  wisl verify $1 -l disabled
}

verify_frac () {
  echo "\nVerifying (frac): $1\n"
  wislf verify $1 -l disabled
}

# Runs an analysis that is EXPECTED to fail (the file contains deliberately
# failing specs/lemmas, or demonstrates a bug) and errors out if it
# unexpectedly succeeds. $1: file, $2: command (e.g. "wisl verify").
expect_fail () {
  echo "\nExpecting failure of: $2 $1\n"
  if $2 $1 -l disabled; then
    echo "ERROR: expected $1 to fail, but it succeeded"
    exit 1
  else
    echo "(failed as expected)"
  fi
}

echo "--- WISL: OX VERIFICATION ---"
verify wisl/examples/SLL_recursive.wisl
verify wisl/examples/SLL_iterative.wisl
verify wisl/examples/DLL_recursive.wisl
verify wisl/examples/loop.wisl
verify wisl/examples/tree.wisl
verify wisl/examples/SLL_ex_complete.wisl
verify wisl/examples/SLL_ex_ongoing.wisl

echo "\n\n--- WISL: FRACTIONAL-PERMISSION VERIFICATION ---"
verify_frac wisl/examples/frac/concurrent_binary_tree.wisl
verify_frac wisl/examples/frac/floating_point.wisl
verify_frac wisl/examples/frac/concrete_test.wisl
verify_frac wisl/examples/frac/lambda_terms.wisl

echo "\n\n--- WISL: EXPECTED-FAILURE TESTS (deliberately failing specs / bugs) ---"
# wand.wisl contains 'should_fail' / 'pure_wand_fail' lemmas.
expect_fail wisl/examples/wand.wisl "wisl verify"
# simple_concurrency.wisl contains 'SHOULD_FAIL_*' procedures.
expect_fail wisl/examples/frac/simple_concurrency.wisl "wislf verify"
# llen_wpst.wisl asserts a property that is violated for some inputs.
expect_fail wisl/examples/wpst/llen_wpst.wisl "wisl wpst"

# echo "\n\n--- WISL: EXACT VERIFICATION ---"
# verify_exv wisl/examples/SLL_ex_ongoing.wisl

exit $FINAL_RETURN
