(** {2 JSIL Basic Commmands} **)
open Gillian.Gil_syntax

type t =
  | Skip  (** Empty command *)
  | Assignment of Var.t * Expr.t  (** Assignment *)
  | New of Var.t * Expr.t option * Expr.t option  (** Object creation *)
  | Lookup of Var.t * Expr.t * Expr.t  (** Field lookup *)
  | Mutation of Expr.t * Expr.t * Expr.t  (** Field mutation *)
  | Delete of Expr.t * Expr.t  (** Field deletion *)
  | DeleteObj of Expr.t  (** Object deletion *)
  | HasField of Var.t * Expr.t * Expr.t  (** Field check *)
  | GetFields of Var.t * Expr.t  (** All fields of an object *)
  | MetaData of Var.t * Expr.t  (** Object metadata *)

let pp fmt bcmd =
  match bcmd with
  | Skip -> Fmt.pf fmt "skip"
  | Assignment (var, e) -> Fmt.pf fmt "%a := %a" Var.pp var Expr.pp e
  | New (var, loc, metadata) ->
      Fmt.pf fmt "%a := new(%a%a%a)" Var.pp var (Fmt.option Expr.pp) loc
        (fun f a -> if Option.is_some a then Fmt.string f ", ")
        loc (Fmt.option Expr.pp) metadata
  | Lookup (var, e1, e2) ->
      Fmt.pf fmt "%a := [%a, %a]" Var.pp var Expr.pp e1 Expr.pp e2
  | Mutation (e1, e2, e3) ->
      Fmt.pf fmt "[%a, %a] := %a" Expr.pp e1 Expr.pp e2 Expr.pp e3
  | Delete (e1, e2) -> Fmt.pf fmt "delete (%a, %a)" Expr.pp e1 Expr.pp e2
  | DeleteObj e1 -> Fmt.pf fmt "deleteObject (%a)" Expr.pp e1
  | HasField (x, e1, e2) ->
      Fmt.pf fmt "%a := hasField(%a, %a)" Var.pp x Expr.pp e1 Expr.pp e2
  | GetFields (x, e) -> Fmt.pf fmt "%a := getFields (%a)" Var.pp x Expr.pp e
  | MetaData (x, e) -> Fmt.pf fmt "%a := metadata (%a)" Var.pp x Expr.pp e
