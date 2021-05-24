module CLI =
  Gillian.CommandLine.Make (Semantics.Concrete) (Semantics.Symbolic)
    (Semantics.External)
    (Js2jsil_lib.JS2GIL_ParserAndCompiler)
    (struct
      let runners : Gillian.Bulk.Runner.t list =
        [ (module Test262.Test262_runner); (module CosetteRunner) ]
    end)
    (Debugger.DisplayFilterMap.Default)
    (Debugging.JSILSMemoryDisplayable)
    (Debugger.MemoryErrorLifter.Dummy
       (Semantics.Symbolic)
       (Js2jsil_lib.JS2GIL_ParserAndCompiler))

let () = CLI.main ()
