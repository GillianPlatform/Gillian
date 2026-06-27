open Utils
open Gil_syntax
module Delayed = Gillian.Monadic.Delayed

(* Make the default value null *)
module ExclusiveNull = struct
  include Exclusive

  let instantiate = function
    | [] -> (Some (Expr.Lit Null), [])
    | [ v ] -> (Some v, [])
    | _ -> failwith "ExclusiveNull: instantiate: too many arguments"
end

module BaseMemory = OpenPMap (LocationIndex) (Freeable (MList (ExclusiveNull)))
module ALocMemory = OpenALocPMap (Freeable (MList (ExclusiveNull)))

module SplitMemory =
  OpenSplitPMap (LocationIndex) (Freeable (MList (ExclusiveNull)))

module WISLSubst : NameMap = struct
  let action_substitutions =
    [
      ("alloc", "alloc");
      ("dispose", "free");
      ("setcell", "store");
      ("getcell", "load");
    ]

  let pred_substitutions =
    [ ("cell", "ex"); ("freed", "freed"); ("bound", "length") ]
end

(* Actual Exports *)

module ParserAndCompiler = WParserAndCompiler

module ExternalSemantics =
  Gillian.General.External.Dummy (WParserAndCompiler.Annot)

module MonadicSMemory_Base = Mapper (WISLSubst) (BaseMemory)
module MonadicSMemory_ALoc = Mapper (WISLSubst) (ALocMemory)
module MonadicSMemory_Split = Mapper (WISLSubst) (SplitMemory)
