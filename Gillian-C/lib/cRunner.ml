open Gillian
module Outcome =
  Bulk.Outcome.Make_Concrete (CMemory) (ParserAndCompiler)
    (General.External.Dummy)

module Suite = struct
  include Bulk.Suite.ByFolder (struct
    let max_depth = 1

    let cmd_name = "bulk-exec"

    let exec_mode = Gillian.Utils.ExecMode.Concrete
  end)

  let filter_source s = String.equal (Filename.extension s) ".c"

  let beforeEach () =
    beforeEach ();
    Generators.reset ()
end

module Expectations = struct
  type matcher = Bulk_rely.OutcomeExt.Make(Outcome).ext Rely.matchers

  type outcome = Outcome.t

  type category = Suite.category

  type info = Suite.info

  let return_value_is_zero ret _ =
    let open Gillian.Gil_syntax.Literal in
    match ret with
    | LList [ String "int"; Num 0. ] -> true
    | _ -> false

  let expectation (expect : matcher) _ outcome =
    (expect.ext.outcome outcome).exactlyOneBranch.toFinishInNormalModeWith
      ~constraint_name:"Returning 0" return_value_is_zero
end

include Bulk_rely.RelyRunner.Make (Outcome) (Suite) (Expectations)
