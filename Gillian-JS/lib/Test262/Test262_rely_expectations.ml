type matcher =
  Gillian_bulk_alcotest.AlcotestCheckers.Make(Test262_outcome).matcher

type outcome = Test262_outcome.t

type category = Test262_suite.category

type info = Test262_suite.info

open Test262_expectations_helper

let expectation
    (expect : matcher)
    (test : (Test262_suite.info, Test262_suite.category) Bulk.Test.t)
    outcome =
  let Test262_suite.{ tt; ept; _ } = test.info in
  match tt with
  | Positive -> expect.finish_in_normal_mode ExactlyOne outcome
  | Negative -> (
      match ept with
      | None ->
          failwith
            "Test262: Malformed test: Negative test without error information"
      | Some (Parse, _) | Some (Early, _) ->
          expect.fail_at_parsing_with
            ~constraint_name:"failing with a Syntax or Reference error"
            parsing_failure_is_jsparser outcome
      | Some (Runtime, et) -> (
          match et with
          | Test262Error   ->
              expect.finish_in_error_mode_with ExactlyOne
                ~constraint_name:"to be a Test262 error" is_test262_error
                outcome
          | SyntaxError    ->
              expect.finish_in_error_mode_with ExactlyOne
                ~constraint_name:"to be a syntax error" is_syntax_error outcome
          | ReferenceError ->
              expect.finish_in_error_mode_with ExactlyOne
                ~constraint_name:"to be a reference error" is_ref_error outcome
          | _              -> failwith "Test262: Unhandled runtime error")
      | Some (Resolution, _) -> failwith "Unsuported phase 'Resolution'")
