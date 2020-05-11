type matcher =
  Gillian_bulk_rely.OutcomeExt.Make(Test262_outcome).ext Rely.matchers

type outcome = Test262_outcome.t

type category = Test262_suite.category

type info = Test262_suite.info

open Test262_expectations_helper

let expectation
    (expect : matcher)
    (test : (Test262_suite.info, Test262_suite.category) Bulk.Test.t)
    outcome =
  let Test262_suite.{ tt; tm; rm; ept } = test.info in
  match tt with
  | Positive ->
      (expect.ext.outcome outcome).exactlyOneBranch.toFinishInNormalMode ()
  | Negative -> (
      match ept with
      | None ->
          failwith
            "Test262: Malformed test: Negative test without error information"
      | Some (Parse, _) | Some (Early, _) ->
          (expect.ext.outcome outcome).toFailAtParsingWith
            ~constraint_name:"failing with a Syntax or Reference error"
            parsing_failure_is_jsparser
      | Some (Runtime, et) -> (
          match et with
          | Test262Error   ->
              (expect.ext.outcome outcome).exactlyOneBranch
                .toFinishInErrorModeWith
                ~constraint_name:"to be a Test262 error" is_test262_error
          | SyntaxError    ->
              (expect.ext.outcome outcome).exactlyOneBranch
                .toFinishInErrorModeWith ~constraint_name:"to be a syntax error"
                is_syntax_error
          | ReferenceError ->
              (expect.ext.outcome outcome).exactlyOneBranch
                .toFinishInErrorModeWith
                ~constraint_name:"to be a reference error" is_ref_error
          | _              -> failwith "Test262: Unhandled runtime error" )
      | Some (Resolution, et) -> failwith "Unsuported phase 'Resolution'" )
