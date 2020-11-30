(** {2 JSIL Basic Commmands} **)
open Gillian.Gil_syntax

type t =
  | Skip  (** Empty command *)
  | Assignment of string * Expr.t  (** Assignment *)
  | New        of string * Expr.t option * Expr.t option  (** Object creation *)
  | Lookup     of string * Expr.t * Expr.t  (** Field lookup *)
  | Mutation   of Expr.t * Expr.t * Expr.t  (** Field mutation *)
  | Delete     of Expr.t * Expr.t  (** Field deletion *)
  | DeleteObj  of Expr.t  (** Object deletion *)
  | HasField   of string * Expr.t * Expr.t  (** Field check *)
  | GetFields  of string * Expr.t  (** All fields of an object *)
  | MetaData   of string * Expr.t  (** Object metadata *)

let rec pp fmt bcmd =
  match bcmd with
  | Skip                     -> Fmt.pf fmt "skip"
  | Assignment (var, e)      -> Fmt.pf fmt "%s := %a" var Expr.pp e
  | New (var, loc, metadata) ->
      Fmt.pf fmt "%s := new(%a%a%a)" var (Fmt.option Expr.pp) loc
        (fun f a -> if Option.is_some a then Fmt.string f ", ")
        loc (Fmt.option Expr.pp) metadata
  | Lookup (var, e1, e2)     ->
      Fmt.pf fmt "%s := [%a, %a]" var Expr.pp e1 Expr.pp e2
  | Mutation (e1, e2, e3)    ->
      Fmt.pf fmt "[%a, %a] := %a" Expr.pp e1 Expr.pp e2 Expr.pp e3
  | Delete (e1, e2)          -> Fmt.pf fmt "delete (%a, %a)" Expr.pp e1 Expr.pp
                                  e2
  | DeleteObj e1             -> Fmt.pf fmt "deleteObject (%a)" Expr.pp e1
  | HasField (x, e1, e2)     ->
      Fmt.pf fmt "%s := hasField(%a, %a)" x Expr.pp e1 Expr.pp e2
  | GetFields (x, e)         -> Fmt.pf fmt "%s := getFields (%a)" x Expr.pp e
  | MetaData (x, e)          -> Fmt.pf fmt "%s := metadata (%a)" x Expr.pp e
