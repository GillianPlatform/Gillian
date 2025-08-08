open States
(* Uncomment to import transformer shorthands
   open Prebuilt.Utils *)

(* Select prebuilt mode (or build one!) -- available state models are C, JS and WISL. *)
module Prebuilt = Prebuilt.Lib.C_Base

(* State model
   For a linear heap, for example:
   module MyMem = OpenPMap (IntegerIndex) (Freeable (Exclusive)) *)
module MyMem = Prebuilt.MonadicSMemory

(* Get modules *)
module PC = Prebuilt.ParserAndCompiler
module ExternalSemantics = Prebuilt.ExternalSemantics
module InitData = Prebuilt.InitData

(* For debugging actions / predicates, uncomment: *)
(* module Debug = Debug.Make (MyMem)
   let () = Debug.print_info () *)

(* Convert custom state model -> Gillian state model *)
module PatchedMem = MyMonadicSMemory.Make (MyMem) (Prebuilt.MyInitData)

(* Gillian Instantiation *)
(* For measuring performance, wrap this in PerfMeasurer.Make *)
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (PatchedMem)

module Lifter
    (Verifier : Gillian.Abstraction.Verifier.S with type annot = PC.Annot.t) =
  Gillian.Debugger.Lifter.Gil_lifter.Make (SMemory) (PC) (Verifier)

module CLI =
  Gillian.Command_line.Make (InitData) (Cmemory.Make (InitData)) (SMemory) (PC)
    (ExternalSemantics)
    (struct
      let runners = []
    end)
    (Lifter)

let () = CLI.main ()
