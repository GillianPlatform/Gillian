open Gillian.Debugger
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (WStateEx.WislSMemory)

module Lifter =
  Lifter.Gil_fallback_lifter.Make (SMemory) (WParserAndCompiler)
    (WDebugging.WislLifter.Make)

module CLI =
  Gillian.Command_line.Make
    (Gillian.General.Init_data.Dummy)
    (WStateConcrete.WislCMemory)
    (SMemory)
    (WParserAndCompiler)
    (Gillian.General.External.Dummy (WParserAndCompiler.Annot))
    (Gillian.Bulk.Runner.DummyRunners)
    (Lifter)

let () = CLI.main ()
