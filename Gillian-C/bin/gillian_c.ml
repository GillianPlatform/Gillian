open Cgil_lib
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (MonadicSMemory)

module CLI =
  Gillian.CommandLine.Make (CMemory) (SMemory) (External.M) (ParserAndCompiler)
    (struct
      let runners : Gillian.Bulk.Runner.t list =
        [ (module CRunner); (module SRunner) ]
    end)

let () = CLI.main ()
