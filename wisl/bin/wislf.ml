open Gillian.Debugger
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (WStateFrac.WislSMemory)

module Lifter =
  Lifter.Gil_fallback_lifter.Make (SMemory) (WParserAndCompiler)
    (WStateFrac.WislLifter.Make)

module CLI =
  Gillian.Command_line.Make
    (Gillian.General.Init_data.Dummy)
    (WStateConcrete.WislCMemory)
    (SMemory)
    (WParserAndCompiler)
    (Gillian.General.External.Dummy (WParserAndCompiler.Annot))
    (Gillian.Bulk.Runner.DummyRunners)
    (Lifter)

let () =
  WUtils.WConfig.fractional_permissions := true;
  CLI.main ()
