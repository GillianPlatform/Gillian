open Prebuilt.Lib.WISL_Split
module PatchedMem = States.MyMonadicSMemory.Make (MonadicSMemory) (MyInitData)
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (PatchedMem)

module Lifter =
  Gillian.Debugger.Lifter.Gil_lifter.Make (SMemory) (ParserAndCompiler)

module CLI =
  Gillian.Command_line.Make (InitData) (States.Cmemory.Make (InitData))
    (SMemory)
    (ParserAndCompiler)
    (ExternalSemantics)
    (struct
      let runners = []
    end)
    (Lifter)

let () = CLI.main ()
