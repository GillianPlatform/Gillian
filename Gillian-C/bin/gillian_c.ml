open Cgil_lib
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (MonadicSMemory)

module CLI =
  Gillian.CommandLine.Make (CMemory) (SMemory) (External.M) (ParserAndCompiler)
    (struct
      let runners : Gillian.Bulk.Runner.t list =
        [ (module CRunner); (module SRunner) ]
    end)
    (Debugger.StoreAndSMemoryLifter.Default (SMemory))
    (Debugger.MemoryErrorLifter.Dummy (SMemory) (ParserAndCompiler))

let () = CLI.main ()
