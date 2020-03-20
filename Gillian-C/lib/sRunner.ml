open Gillian
module Outcome =
  Bulk.Outcome.Make_Symbolic (SMemory) (ParserAndCompiler)
    (General.External.Dummy)

module Suite = struct
  include Bulk.Suite.ByFolder (struct
    let max_depth = 1

    let cmd_name = "bulk-wpst"

    let exec_mode = Gillian.Utils.ExecMode.Symbolic
  end)

  let contains_substring s1 s2 =
    let re = Str.regexp_string s2 in
    try
      ignore (Str.search_forward re s1 0);
      true
    with Not_found -> false

  let filter_source s =
    Filename.check_suffix s ".c"
    && not (!Gillian.Utils.Config.ci && contains_substring s "bug/")
end

module Expectations = struct
  type matcher = Bulk_rely.OutcomeExt.Make(Outcome).ext Rely.matchers

  type outcome = Outcome.t

  type category = Suite.category

  type info = Suite.info

  let expectation (expect : matcher) _ outcome =
    (expect.ext.outcome outcome).allBranches.toFinishInNormalMode ()
end

include Bulk_rely.RelyRunner.Make (Outcome) (Suite) (Expectations)
