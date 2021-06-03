module CLI =
  Gillian.CommandLine.Make (Semantics.Concrete) (Semantics.Symbolic)
    (Semantics.External)
    (Js2jsil_lib.JS2GIL_ParserAndCompiler)
    (struct
      let runners : Gillian.Bulk.Runner.t list =
        [ (module Test262.Test262_runner); (module CosetteRunner) ]
    end)
    (Debugger.StoreAndSMemoryLifter.Default (Semantics.Symbolic))
    (Debugger.MemoryErrorLifter.Dummy
       (Semantics.Symbolic)
       (Js2jsil_lib.JS2GIL_ParserAndCompiler))

let () = CLI.main ()
