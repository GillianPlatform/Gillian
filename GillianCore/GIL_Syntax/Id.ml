open Allocators

type +'a t = string
and loc = [ `Loc ]
and aloc = [ `ALoc ]
and var = [ `Var ]
and lvar = [ `LVar ] [@@deriving yojson]

type any_var = [ var | lvar ]
and any_loc = [ loc | aloc ]
and any = [ loc | aloc | var | lvar ]
and substable = [ aloc | var | lvar ] [@@deriving yojson]

let return_variable = Names.return_variable
let str : 'a t -> string = Fun.id
let equal : 'a t -> 'b t -> bool = String.equal
let pp fmt (i : 'a t) = Fmt.pf fmt "%s" i

let as_lvars (vars : any_var t list) : lvar t list option =
  if List.for_all Names.is_lvar_name vars then Some vars else None

let as_aloc (loc : any_loc t) : aloc t option =
  if Names.is_aloc_name loc then Some loc else None

module type Id = sig
  include S_with_stringify
  module Set = Containers.SS
end

module Make (I : sig
  val prefix : string
end) : Id with type t = string = struct
  include Make_with_prefix (Basic ()) (I)
  module Set = Containers.SS
end

module Loc = Make (struct
  let prefix = Names.lloc_prefix
end)

module ALoc = Make (struct
  let prefix = Names.aloc_prefix
end)

module LVar = Make (struct
  let prefix = Names.lvar_prefix
end)

module Var = Make (struct
  let prefix = Names.pvar_prefix
end)

module Sets = struct
  module SubstSet = Containers.SS
  module LocSet = Containers.SS
  module VarSet = Containers.SS

  let pvar_to_varset = Fun.id
  let lvar_to_varset = Fun.id
  let pvar_to_subst = Fun.id
  let lvar_to_subst = Fun.id
  let aloc_to_subst = Fun.id
  let aloc_to_loc = Fun.id

  (** @deprecated *)
  let substset_to_lvar = Fun.id
end
