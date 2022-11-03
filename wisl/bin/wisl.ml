open Gillian.Debugger

module CLI =
  Gillian.CommandLine.Make
    (Gillian.General.Init_data.Dummy)
    (WSemantics.WislCMemory)
    (WSemantics.WislSMemory)
    (WParserAndCompiler)
    (Gillian.General.External.Dummy (WParserAndCompiler.Annot))
    (Gillian.Bulk.Runner.DummyRunners)
    (Lifter.GilFallbackLifter.Make (WSemantics.WislSMemory) (WParserAndCompiler)
       (WDebugging.WislLifter.Make))

let () = CLI.main ()
