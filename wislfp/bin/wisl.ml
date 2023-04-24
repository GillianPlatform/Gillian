open Gillian.Debugger

module CLI =
  Gillian.Command_line.Make
    (Gillian.General.Init_data.Dummy)
    (WSemantics.WislCMemory)
    (WSemantics.WislSMemory)
    (WParserAndCompiler)
    (Gillian.General.External.Dummy (WParserAndCompiler.Annot))
    (Gillian.Bulk.Runner.DummyRunners)
    (Lifter.Gil_fallback_lifter.Make
       (WSemantics.WislSMemory)
       (WParserAndCompiler)
       (WDebugging.WislLifter.Make))

let () = CLI.main ()
