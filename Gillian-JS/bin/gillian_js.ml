module CLI =
  Gillian.CommandLine.Make
    (Gillian.General.Global_env.Dummy)
    (Semantics.Concrete)
    (Semantics.Symbolic)
    (Semantics.External)
    (Js2jsil_lib.JS2GIL_ParserAndCompiler)
    (struct
      let runners : Gillian.Bulk.Runner.t list =
        [ (module Test262.Test262_runner); (module CosetteRunner) ]
    end)
    (Debugging.JSLifter)

let () = CLI.main ()
