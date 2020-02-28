open Gillian
module Outcome =
  Bulk.Outcome.Make_Symbolic (Semantics.SMemory) (ParserAndCompiler)
    (General.External.Dummy)

module Suite = struct
  include Bulk.Suite.ByFolder (struct
    let max_depth = 1

    let cmd_name = "bulk-wpst"

    let exec_mode = Gillian.Utils.ExecMode.Symbolic
  end)

  let filter_source s =
    Filename.check_suffix s ".c"
    && not (!Gillian.Utils.Config.ci && Filename.check_suffix s "bug/")
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
