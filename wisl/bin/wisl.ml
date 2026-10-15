open Gillian.Debugger

module Lifter =
  Lifter.Gil_fallback_lifter.Make (WStateEx.WislSMemory) (WParserAndCompiler)
    (WStateEx.WislLifter.Make)

module CLI =
  Gillian.Command_line.Make
    (Gillian.General.Init_data.Dummy)
    (WStateConcrete.WislCMemory)
    (WStateEx.WislSMemory)
    (WParserAndCompiler)
    (Gillian.General.External.Dummy (WParserAndCompiler.Annot))
    (Gillian.Bulk.Runner.DummyRunners)
    (Lifter)

let () = CLI.main ()
