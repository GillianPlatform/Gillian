open Memory_model
open Gillian_C2_compiler
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (MonadicSMemory)
module Init_data = Gillian.General.Init_data.Dummy

module Gillian_C2_lifter =
  Gillian.Debugger.Lifter.Gil_fallback_lifter.Make
    (SMemory)
    (C2ParserAndCompiler)
    (Lifter.C2_lifter.Make (SMemory))

module CLI =
  Gillian.Command_line.Make (Init_data) (CMemory) (SMemory)
    (C2ParserAndCompiler)
    (External.M)
    (struct
      let runners : Gillian.Bulk.Runner.t list = []
    end)
    (Gillian_C2_lifter)

let () = CLI.main ()
