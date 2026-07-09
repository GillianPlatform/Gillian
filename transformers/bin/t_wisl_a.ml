open Prebuilt.Lib.WISL_ALoc

module SMemory =
  Gillian.Combinators.MyMonadicSMemory.Make (MonadicSMemory) (MyInitData)

module Lifter =
  Gillian.Debugger.Lifter.Gil_lifter.Make (SMemory) (ParserAndCompiler)

module CLI =
  Gillian.Command_line.Make
    (InitData)
    (Gillian.Combinators.Cmemory.Make (InitData))
    (SMemory)
    (ParserAndCompiler)
    (ExternalSemantics)
    (struct
      let runners = []
    end)
    (Lifter)

let () = CLI.main ()
