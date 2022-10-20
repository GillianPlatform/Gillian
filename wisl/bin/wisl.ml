module CLI =
  Gillian.CommandLine.Make
    (Gillian.General.Init_data.Dummy)
    (WSemantics.WislCMemory)
    (WSemantics.WislSMemory)
    (Gillian.General.External.Dummy)
    (WParserAndCompiler)
    (Gillian.Bulk.Runner.DummyRunners)
    (WDebugging.WislLifter)

let () = CLI.main ()
