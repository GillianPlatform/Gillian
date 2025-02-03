open Gillian
open Gillian.Utils
open Gil_syntax
open Llvm_memory_model.Memories
module DummyParserAndCompiler = ParserAndCompiler.Dummy

module Outcome =
  Bulk.Outcome.Make_Symbolic (SMemory) (NoopParser)
    (General.External.Dummy (Gil_syntax.Annot.Basic))

module Suite = struct
  include Bulk.Suite.ByFolder (struct
    let max_depth = 1
    let cmd_name = "bulk-wpst"
    let exec_mode = Gillian.Utils.Exec_mode.Symbolic
  end)

  let contains_substring s1 s2 =
    let re = Str.regexp_string s2 in
    try
      ignore (Str.search_forward re s1 0);
      true
    with Not_found -> false

  let filter_source s =
    Filename.check_suffix s ".gil"
    && not (!Gillian.Utils.Config.ci && contains_substring s "bug/")

  let beforeEach () = Generators.reset ()
end

module Expectations = struct
  type matcher = Alcotest_runner.AlcotestCheckers.Make(Outcome).matcher
  type outcome = Outcome.t
  type category = Suite.category
  type info = Suite.info

  let expectation (expect : matcher) (test : (info, string) Bulk.Test.t) outcome
      =
    let cat = test.category in
    match cat with
    | "fail" -> expect.finish_in_fail outcome
    | "succeed" -> expect.finish_in_normal_mode AllOfThem outcome
    | _ -> failwith "Unknown category"
end

include Alcotest_runner.AlcotestRunner.Make (Outcome) (Suite) (Expectations)
