open Cgil_lib

module CLI =
  Gillian.CommandLine.Make (Semantics.CMemory) (Semantics.SMemory) (External.M)
    (ParserAndCompiler)
    (struct
      let runners : Gillian.Bulk.Runner.t list =
        [ (module CRunner); (module SRunner) ]
    end)

let () = CLI.main ()
