open Utils
open Gil_syntax
module Delayed = Gillian.Monadic.Delayed

(* Make the default value null *)
module FractionalNull = struct
  include Fractional

  let instantiate = function
    | [] -> (Some (Expr.Lit Null, Expr.num 1.0), [])
    | [ v ] -> (Some (v, Expr.num 1.0), [])
    | _ -> failwith "FractionalNull: instantiate: too many arguments"
end

module BaseMemory = OpenPMap (LocationIndex) (Freeable (MList (FractionalNull)))
module ALocMemory = OpenALocPMap (Freeable (MList (FractionalNull)))

module SplitMemory =
  OpenSplitPMap (LocationIndex) (Freeable (MList (FractionalNull)))

module WISLSubst : NameMap = struct
  let action_substitutions =
    [
      ("alloc", "alloc");
      ("dispose", "free");
      ("setcell", "store");
      ("getcell", "load");
    ]

  let pred_substitutions =
    [ ("cell", "frac"); ("freed", "freed"); ("bound", "length") ]
end

(* Actual Exports *)

module ParserAndCompiler = WParserAndCompiler

module ExternalSemantics =
  Gillian.General.External.Dummy (WParserAndCompiler.Annot)

module MonadicSMemory_Base = Mapper (WISLSubst) (BaseMemory)
module MonadicSMemory_ALoc = Mapper (WISLSubst) (ALocMemory)
module MonadicSMemory_Split = Mapper (WISLSubst) (SplitMemory)
