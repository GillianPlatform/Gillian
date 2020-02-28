module Make (Outcome : Outcome.S) (Suite : Suite.S) = struct
  module TestFramework = RelyFramework.Make (Outcome) (Suite)

  type matcher = OutcomeExt.Make(Outcome).ext Rely.matchers

  type category = Suite.category

  let register_expectation_for_one_test
      expectation test_executor testFn source test =
    testFn source (fun { Rely.expect } ->
        let result = test_executor expect test in
        expectation expect test result)

  let register_expectations_for_category ~expectation ~test_runner cat tbl =
    if Suite.skip_category cat then
      TestFramework.describeSkip (Fmt.str "%a" Suite.pp_category cat) (function
          | { test } ->
          Hashtbl.iter
            (register_expectation_for_one_test expectation test_runner test)
            tbl)
    else
      TestFramework.describe (Fmt.str "%a" Suite.pp_category cat) (function
          | { test } ->
          Hashtbl.iter
            (register_expectation_for_one_test expectation test_runner test)
            tbl)

  let check_not_throw (expect : matcher) func = (expect.fn func).not.toThrow ()

  let run = TestFramework.run
end
