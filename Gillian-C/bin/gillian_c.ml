open Cgil_lib
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (MonadicSMemory)
module SMemoryDisplayable = SMemoryDisplayable.Make (SMemory)

module CLI =
  Gillian.CommandLine.Make (CMemory) (SMemory) (External.M) (ParserAndCompiler)
    (struct
      let runners : Gillian.Bulk.Runner.t list =
        [ (module CRunner); (module SRunner) ]
    end)
    (Debugger.DisplayFilterMap.Default)
    (SMemoryDisplayable)
    (Debugger.MemoryErrorLifter.Dummy (SMemory))

let () = CLI.main ()
