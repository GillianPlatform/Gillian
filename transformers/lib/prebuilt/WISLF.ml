open Utils
open Gil_syntax
module Delayed = Gillian.Monadic.Delayed
module Frac_used = Fractional.Frac_with_wildcard

(* Make the default value null *)
module FractionalNull = struct
  include Fractional.Make (Frac_used)

  let instantiate = function
    | [] -> (Some (Expr.Lit Null, Frac_used._1), [])
    | [ v ] -> (Some (v, Frac_used._1), [])
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
