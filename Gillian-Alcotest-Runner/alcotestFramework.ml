open Utils.Gillian_result.Error

module Make (Outcome : Outcome.S) = struct
  include AlcotestCheckers.Make (Outcome)

  let make_check_fail_at_parsing
      ~(expected : (string * (compilation_error -> bool)) option)
      (actual : Outcome.t) : unit =
    match (actual, expected) with
    | Error (CompilationError _), None -> ()
    | Error (CompilationError pa), Some (cname, constr) ->
        let failure_message =
          Fmt.str
            "Expected test to fail at parsing time with %s\n\
             but it failed at parsing time with constraint: %a" pa.msg
            Fmt.string cname
        in
        Alcotest.(check bool) failure_message true (constr pa)
    | other, _ ->
        Alcotest.failf "Expected test %a\nBut test actually %a"
          (Fmt.option
             ~none:(fun f () -> Fmt.pf f "to fail at parsing time")
             (fun f (constr_name, _) ->
               Fmt.pf f "to fail at parsing time with constraint: %s"
                 constr_name))
          expected Outcome.pp_what_test_did other

  let make_check_finish_in_mode
      ~(flag : Flag.t)
      ~(expected : (string * (Outcome.Val.t -> Outcome.State.t -> bool)) option)
      (branches : BranchReasoning.branches)
      (actual : Outcome.t) =
    match actual with
    | Ok res ->
        let msg, pass =
          BranchReasoning.resInMode
            ~pp_what_branch_did:Outcome.pp_what_branch_did branches flag
            expected res
        in
        if pass then () else Alcotest.fail (msg ())
    | other ->
        Alcotest.failf
          "Expected test to finish successfully in normal mode\n\
           But test actually %a" Outcome.pp_what_test_did other

  let fail_at_parsing = make_check_fail_at_parsing ~expected:None

  let fail_at_parsing_with ~constraint_name constr =
    make_check_fail_at_parsing ~expected:(Some (constraint_name, constr))

  let fail_at_exec = function
    | Error (InternalError _ | OperationError _ | AnalysisFailures _) -> ()
    | (Ok _ | Error (CompilationError _)) as actual ->
        Alcotest.failf
          "Expected the test to fail at execution \nBut the test %a"
          Outcome.pp_what_test_did actual

  let finish_in_error_mode =
    make_check_finish_in_mode ~flag:Flag.Error ~expected:None

  let finish_in_error_mode_with branches ~constraint_name constr =
    make_check_finish_in_mode ~flag:Flag.Error
      ~expected:(Some (constraint_name, constr))
      branches

  let finish_in_normal_mode =
    make_check_finish_in_mode ~flag:Flag.Normal ~expected:None

  let finish_in_normal_mode_with branches ~constraint_name constr =
    make_check_finish_in_mode ~flag:Flag.Normal
      ~expected:(Some (constraint_name, constr))
      branches

  let custom_checkers =
    {
      fail_at_parsing;
      fail_at_parsing_with;
      fail_at_exec;
      finish_in_error_mode;
      finish_in_error_mode_with;
      finish_in_normal_mode;
      finish_in_normal_mode_with;
    }
end
