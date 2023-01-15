open Gillian

module Outcome =
  Bulk.Outcome.Make_Symbolic
    (Semantics.Symbolic)
    (Js2jsil_lib.JS2GIL_ParserAndCompiler)
    (General.External.Dummy (Gil_syntax.Annot.Basic))

module Suite = struct
  include Bulk.Suite.ByFolder (struct
    let max_depth = 1
    let cmd_name = "cosette-bulk"
    let exec_mode = Gillian.Utils.Exec_mode.Symbolic
  end)

  let contains_bug s =
    try
      let _ = Str.search_forward (Str.regexp_string "/bug/") s 0 in
      true
    with Not_found -> false

  let filter_source s =
    Filename.check_suffix s ".js"
    && not (!Gillian.Utils.Config.ci && contains_bug s)
end

module Expectations = struct
  type matcher = Alcotest_runner.AlcotestCheckers.Make(Outcome).matcher
  type outcome = Outcome.t
  type category = Suite.category
  type info = Suite.info

  let expectation (expect : matcher) _ outcome =
    expect.finish_in_normal_mode AllOfThem outcome
end

include Alcotest_runner.AlcotestRunner.Make (Outcome) (Suite) (Expectations)
