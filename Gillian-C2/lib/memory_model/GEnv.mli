type err_t = Symbol_not_found of string [@@deriving show, yojson]

module Id := Gil_syntax.Id
module StringMap : Map.S with type key = string
module LocMap : Map.S with type key = Id.any_loc Id.t

module Concrete : sig
  open Gil_syntax

  type nonrec err_t = err_t [@@deriving show]
  type def = FunDef of string | GlobVar of string

  val serialize_def : def -> Literal.t
  val deserialize_def : Literal.t -> def

  type t = {
    symb : Id.any_loc Id.t StringMap.t;  (** maps symbols to loc names *)
    defs : def LocMap.t;  (** maps loc names to definitions *)
  }

  (** Finds a location name given symbol in the global environment *)
  val find_symbol : t -> string -> (Id.any_loc Id.t, err_t) result

  (** Finds a definition given its location name in the global environment *)
  val find_def : t -> Id.any_loc Id.t -> def

  (** [set_symbol genv symbol locname ]
      Returns a new global environment where the symbol [symbol] is associated with the location [locname] *)
  val set_symbol : t -> string -> Id.any_loc Id.t -> t

  (** [set_def genv locname def ]
      Returns a new global environment where the block [locname] is associated with the global definition [def] *)
  val set_def : t -> Id.any_loc Id.t -> def -> t

  (** Empty global environment *)
  val empty : t

  (** Pretty printer for the global environment *)
  val pp : Format.formatter -> t -> unit

  (** {3 Symbolic things} *)

  val substitution : Gillian.Symbolic.Subst.t -> t -> t
  val assertions : t -> Id.any_loc Id.t list * Gillian.Gil_syntax.Asrt.t
end

module Symbolic : sig
  open Gil_syntax

  type nonrec err_t = err_t [@@deriving show, yojson]
  type def = FunDef of Expr.t | GlobVar of Expr.t

  val serialize_def : def -> Expr.t
  val deserialize_def : Expr.t -> def

  type t = {
    symb : Id.any_loc Id.t StringMap.t;  (** maps symbols to loc names *)
    defs : def LocMap.t;  (** maps loc names to definitions *)
  }

  (** Finds a location name given symbol in the global environment *)
  val find_symbol : t -> string -> (Id.any_loc Id.t, err_t) result

  (** Finds a definition given its location name in the global environment *)
  val find_def : t -> Id.any_loc Id.t -> def

  (** [set_symbol genv symbol locname ]
      Returns a new global environment where the symbol [symbol] is associated with the location [locname] *)
  val set_symbol : t -> string -> Gil_syntax.Expr.t -> t Monadic.Delayed.t

  (** [set_def genv locname def ]
      Returns a new global environment where the block [locname] is associated with the global definition [def] *)
  val set_def : t -> Id.any_loc Id.t -> def -> t Monadic.Delayed.t

  (** Empty global environment *)
  val empty : t

  (** Pretty printer for the global environment *)
  val pp : Format.formatter -> t -> unit

  (** {3 Symbolic things} *)

  val substitution : Gillian.Symbolic.Subst.t -> t -> t
  val assertions : t -> Id.any_loc Id.t list * Gillian.Gil_syntax.Asrt.t
end
