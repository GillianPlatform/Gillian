module Make (Outcome : Outcome.S) (Suite : Suite.S) = struct
  include Rely.Make (struct
    let config =
      Rely.TestFrameworkConfig.initialize
        {
          snapshotDir = "path/to/test/lib/__snapshots__";
          projectDir = "path/to/your/project";
        }
  end)

  include OutcomeExt.Make (Outcome)
  open Bulk.BranchReasoning

  let makeFormater format pp fmt e =
    Fmt.pf fmt "%s" (format (Fmt.str "%a" pp e))

  let createFailAtParsingMatcher
      (createMatcher : ('a, 'b) Rely.MatcherTypes.createMatcher) =
    let open Rely.MatcherTypes in
    let open Rely.MatcherUtils in
    createMatcher
      (fun { formatReceived; formatExpected } actualThunk expectedThunk ->
        let formatReceived pp fmt x = (makeFormater formatReceived) pp fmt x in
        let formatExpected pp fmt x = (makeFormater formatExpected) pp fmt x in
        let (actual : Outcome.t) = actualThunk () in
        let (expected
              : (string * (Outcome.ParserAndCompiler.err -> bool)) option) =
          expectedThunk ()
        in
        match (actual, expected) with
        | ParseAndCompileError _, None -> ((fun () -> ""), true)
        | ParseAndCompileError pa, Some (cname, constr) ->
            if constr pa then ((fun () -> ""), true)
            else
              let failure_message =
                Fmt.str
                  "Expected test to fail at parsing time with %a\n\
                   but it failed at parsing time with constraint: %a"
                  (formatExpected Outcome.ParserAndCompiler.pp_err)
                  pa
                  (formatReceived Fmt.string)
                  cname
              in
              ((fun () -> failure_message), false)
        | other, _ ->
            let failure_message =
              Fmt.str "Expected test %a\nBut test actually %a"
                (formatExpected
                   (Fmt.option
                      ~none:(fun f () -> Fmt.pf f "to fail at parsing time")
                      (fun f (constr_name, _) ->
                        Fmt.pf f "to fail at parsing time with constraint: %s"
                          constr_name)))
                expected
                (formatReceived Outcome.pp_what_test_did)
                other
            in
            ((fun _ -> failure_message), false))

  let createFailAtExecMatcher createMatcher =
    let open Rely.MatcherTypes in
    let open Rely.MatcherUtils in
    createMatcher
      (fun { formatReceived; formatExpected } actualThunk expectedThunk ->
        let formatReceived pp fmt x = (makeFormater formatReceived) pp fmt x in
        let formatExpected pp fmt x = (makeFormater formatExpected) pp fmt x in
        let (actual : Outcome.t) = actualThunk () in
        match actual with
        | Outcome.FailedExec _ -> ((fun () -> ""), true)
        | _                    ->
            let failure_message =
              Fmt.str "Expected the test %a\nBut the test %a"
                (formatExpected (fun f () -> Fmt.pf f "to fail at execution"))
                ()
                (formatReceived Outcome.pp_what_test_did)
                actual
            in
            ((fun () -> failure_message), false))

  let createToFinishInMode
      branches flag (createMatcher : ('a, 'b) Rely.MatcherTypes.createMatcher) =
    let open Rely.MatcherTypes in
    let open Rely.MatcherUtils in
    createMatcher
      (fun { formatReceived = fmtrcvstr; formatExpected = fmtexpstr }
           actualThunk
           expectedThunk
           ->
        let formatReceived pp fmt x = (makeFormater fmtrcvstr) pp fmt x in
        let formatExpected pp fmt x = (makeFormater fmtexpstr) pp fmt x in
        let (actual : Outcome.t) = actualThunk () in
        let (expected
              : (string * (Outcome.Val.t -> Outcome.State.t -> bool)) option) =
          expectedThunk ()
        in
        match actual with
        | FinishedExec res ->
            resInMode ~fmtexp:fmtexpstr ~fmtrcv:fmtrcvstr
              ~pp_what_branch_did:Outcome.pp_what_branch_did branches flag
              expected res
        | other            ->
            let failure_message =
              Fmt.str "Expected test %a\nBut test actually %a"
                (formatExpected (fun f () ->
                     Fmt.pf f "to finish successfully in normal mode"))
                ()
                (formatReceived Outcome.pp_what_test_did)
                other
            in
            ((fun _ -> failure_message), false))

  let resExts nbr actual extendUtils =
    let open Rely.MatcherTypes in
    let { createMatcher } = extendUtils in
    let toFinishInNormalMode () =
      (createToFinishInMode nbr Flag.Normal createMatcher)
        (fun () -> actual)
        (fun () -> None)
    in
    let toFinishInNormalModeWith ~constraint_name constr =
      (createToFinishInMode nbr Flag.Normal createMatcher)
        (fun () -> actual)
        (fun () -> Some (constraint_name, constr))
    in
    let toFinishInErrorMode () =
      (createToFinishInMode nbr Flag.Error createMatcher)
        (fun () -> actual)
        (fun () -> None)
    in
    let toFinishInErrorModeWith ~constraint_name constr =
      (createToFinishInMode nbr Flag.Error createMatcher)
        (fun () -> actual)
        (fun () -> Some (constraint_name, constr))
    in
    {
      toFinishInErrorMode;
      toFinishInErrorModeWith;
      toFinishInNormalMode;
      toFinishInNormalModeWith;
    }

  let outcomeExts actual extendUtils =
    let open Rely.MatcherTypes in
    let { createMatcher } = extendUtils in
    let toFailAtParsing () =
      (createFailAtParsingMatcher createMatcher)
        (fun () -> actual)
        (fun () -> None)
    in
    let toFailAtParsingWith ~constraint_name constr =
      (createFailAtParsingMatcher createMatcher)
        (fun () -> actual)
        (fun () -> Some (constraint_name, constr))
    in
    let toFailAtExec () =
      (createFailAtExecMatcher createMatcher) (fun () -> actual) (fun () -> ())
    in
    let allBranches = resExts AllOfThem actual extendUtils in
    let atLeastOneBranch = resExts AtLeastOne actual extendUtils in
    let exactlyOneBranch = resExts ExactlyOne actual extendUtils in
    {
      toFailAtParsing;
      toFailAtParsingWith;
      toFailAtExec;
      allBranches;
      atLeastOneBranch;
      exactlyOneBranch;
    }

  let customMatchers createMatcher =
    { outcome = (fun o -> outcomeExts o createMatcher) }

  let { describe; describeSkip } =
    describeConfig
    |> withLifecycle (fun tlf -> tlf |> beforeEach Suite.beforeEach)
    |> withCustomMatchers customMatchers
    |> build

  type testUtils = (ext, unit) Rely.testUtils

  let print_failures aggregatedResults =
    let failure_of_suite =
      List.fold_left
        (fun acc r ->
          let open RelyInternal.TestResult.TestSuiteResult in
          if r.numFailedTests > 0 then
            List.fold_left
              (fun accp rp ->
                let open RelyInternal.TestResult in
                match rp.testStatus with
                | Passed _ | Skipped _ -> accp
                | _                    -> rp.title :: accp)
              [] r.testResults
            :: acc
          else acc)
        []
    in
    let failures =
      failure_of_suite
        aggregatedResults
          .RelyInternal.TestResult.AggregatedResult.testSuiteResults
    in
    match failures with
    | []       -> ()
    | failures ->
        Fmt.pr "ALL FAILURES:\n%a\n"
          Fmt.(vbox ~indent:0 (list ~sep:(any "\n") string))
          (List.concat failures)

  let print_failures aggregatedResults =
    if !Utils.Config.bulk_print_all_failures then
      print_failures aggregatedResults
    else ()

  let run () =
    run
      Rely.RunConfig.(
        initialize () |> ciMode true
        |> withReporters
             [
               Default;
               Custom
                 {
                   onTestSuiteStart = (fun _ -> ());
                   onTestSuiteResult = (fun _ _ _ -> ());
                   onRunStart = (fun _ -> ());
                   onRunComplete = print_failures;
                 };
             ])
end
