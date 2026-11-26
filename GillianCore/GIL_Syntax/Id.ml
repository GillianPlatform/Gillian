type +'a t = string
type loc = [ `Loc ]
type aloc = [ `ALoc ]
type var = [ `Var ]
type lvar = [ `LVar ]
type any_var = [ var | lvar ]
type any_loc = [ loc | aloc ]
type substable = [ aloc | var | lvar ]
type any = [ loc | aloc | var | lvar ]

let to_string (x : 'a t) : string = x
let equal = String.equal
let compare = String.compare
let pp = Fmt.string

module type Set_S = sig
  include Set.S

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module type S = sig
  type tag
  type nonrec t = tag t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t Fmt.t
  val show : t -> string
  val of_string : string -> t
  val to_string : t -> string

  module Set : Set_S with type elt = t
  module Map : Map.S with type key = t
end

module type Allocated = sig
  include S

  val alloc : unit -> t
end

module Make (Tag : sig
  type tag
end) : S with type tag = Tag.tag = struct
  type tag = Tag.tag
  type nonrec t = tag t

  let to_yojson x = `String x

  let of_yojson = function
    | `String x -> Ok x
    | _ -> Error "Id.of_yojson: expected a string"

  let equal = equal
  let compare = compare
  let pp fmt x = Fmt.pf fmt "%S" x
  let show x = Fmt.str "%S" x
  let of_string x = x
  let to_string = to_string

  module Set = Containers.SS

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
    let to_yojson = to_yojson
    let of_yojson = of_yojson
  end)
end

module Make_allocated (Tag : sig
  type tag
end) (A : sig
  val alloc : unit -> string
end) : Allocated with type tag = Tag.tag = struct
  include Make (Tag)

  let alloc = A.alloc
end

module Loc =
  Make_allocated
    (struct
      type tag = loc
    end)
    (struct
      let alloc = Generators.fresh_loc
    end)

module ALoc =
  Make_allocated
    (struct
      type tag = aloc
    end)
    (Allocators.Make_with_prefix
       (Allocators.Basic ())
       (struct
         let prefix = Names.aloc_prefix
       end))

module Var = struct
  include
    Make_allocated
      (struct
        type tag = var
      end)
      (struct
        let alloc = Generators.fresh_pvar
      end)

  let return_variable = of_string Names.return_variable
end

module LVar = struct
  include
    Make_allocated
      (struct
        type tag = lvar
      end)
      (Allocators.Make_with_prefix
         (Allocators.Basic ())
         (struct
           let prefix = Names.lvar_prefix
         end))

  let is_spec = Names.is_spec_var_name
  let is_internal = Names.is_internal_lvar_name
end

module Any_var = Make (struct
  type tag = any_var
end)

module Any_loc = Make (struct
  type tag = any_loc
end)

module Substable = Make (struct
  type tag = substable
end)

let classify_var (x : [< any_var ] t) : [ `Var of Var.t | `LVar of LVar.t ] =
  if Names.is_lvar_name x then `LVar x else `Var x

let classify_loc (x : [< any_loc ] t) : [ `Loc of Loc.t | `ALoc of ALoc.t ] =
  if Names.is_aloc_name x then `ALoc x else `Loc x

let as_lvar (x : [< any_var ] t) : LVar.t option =
  if Names.is_lvar_name x then Some x else None

let as_lvars (xs : [< any_var ] t list) : LVar.t list option =
  if List.for_all Names.is_lvar_name xs then Some xs else None
