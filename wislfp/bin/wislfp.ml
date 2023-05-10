open Gillian.Debugger

module SMemory =
Gillian.Symbolic.Legacy_s_memory.Modernize (PwSemantics.WislSMemory)

module CLI =
  Gillian.Command_line.Make
    (Gillian.General.Init_data.Dummy)
    (PwSemantics.WislCMemory)
    (SMemory)
    (PwParserAndCompiler)
    (Gillian.General.External.Dummy (PwParserAndCompiler.Annot))
    (Gillian.Bulk.Runner.DummyRunners)
    (Lifter.Gil_fallback_lifter.Make
       (SMemory)
       (PwParserAndCompiler)
       (PwDebugging.WislLifter.Make))

let () = CLI.main ()
