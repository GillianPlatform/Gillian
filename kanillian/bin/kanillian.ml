open Memory_model
open Kanillian_lib
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (MonadicSMemory)
module Init_data = Gillian.General.Init_data.Dummy

module KaniLifter =
  Gillian.Debugger.Lifter.Gil_fallback_lifter.Make
    (SMemory)
    (KParserAndCompiler)
    (Lifter.Kani_c_lifter.Make (SMemory))

module CLI =
  Gillian.Command_line.Make (Init_data) (CMemory) (SMemory) (KParserAndCompiler)
    (External.M)
    (struct
      let runners : Gillian.Bulk.Runner.t list = []
    end)
    (KaniLifter)

let () = CLI.main ()
