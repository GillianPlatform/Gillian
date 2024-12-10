open Gillian
open States
(* open Prebuilt.Utils*)

(* Select prebuilt mode (or build one!) *)
module Prebuilt = Prebuilt.Lib.C_Base

(* state model: *)
module MyMem = Prebuilt.MonadicSMemory

(* get modules *)
module PC = Prebuilt.ParserAndCompiler
module ExternalSemantics = Prebuilt.ExternalSemantics
module InitData = Prebuilt.InitData

(* Debug *)
(* module Debug = Debug.Make (MyMem)

   let () = Debug.print_info () *)

(* Convert custom memory model -> Gillian memory model *)
module PatchedMem = MyMonadicSMemory.Make (MyMem) (Prebuilt.MyInitData)

(* Gillian Instantiation *)
module SMemory =
  PerfMeasurer.Make (Gillian.Monadic.MonadicSMemory.Lift (PatchedMem))

module Lifter
    (Verifier : Gillian.Abstraction.Verifier.S
                  with type annot = Gil_syntax.Annot.Basic.t) =
  Gillian.Debugger.Lifter.Gil_lifter.Make (SMemory) (PC) (Verifier)

module CLI =
  Gillian.Command_line.Make (InitData) (Cmemory.Make (InitData)) (SMemory) (PC)
    (ExternalSemantics)
    (struct
      let runners = []
    end)
    (Lifter)

let () = CLI.main ()
