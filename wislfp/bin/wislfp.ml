open Gillian.Debugger

module CLI =
  Gillian.Command_line.Make
    (Gillian.General.Init_data.Dummy)
    (PwSemantics.WislCMemory)
    (PwSemantics.WislSMemory)
    (PwParserAndCompiler)
    (Gillian.General.External.Dummy (PwParserAndCompiler.Annot))
    (Gillian.Bulk.Runner.DummyRunners)
    (Lifter.Gil_fallback_lifter.Make
       (PwSemantics.WislSMemory)
       (PwParserAndCompiler)
       (PwDebugging.WislLifter.Make))

let () = CLI.main ()