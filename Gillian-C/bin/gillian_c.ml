open Cgil_lib

module CLI =
  Gillian.CommandLine.Make (Semantics.CMemory) (Semantics.SMemory)
    (Gillian.General.External.Dummy)
    (ParserAndCompiler)
    (struct
      let runners : Gillian.Bulk.Runner.t list =
        [ (module CRunner); (module SRunner) ]
    end)

let () = CLI.main ()
