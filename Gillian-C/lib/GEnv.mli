type err_t = Symbol_not_found of string

module Concrete : sig
  open Gil_syntax

  type nonrec err_t = err_t
  type def = FunDef of string | GlobVar of string

  val serialize_def : def -> Literal.t
  val deserialize_def : Literal.t -> def

  type t = {
    symb : (string, string) PMap.t;  (** maps symbols to loc names *)
    defs : (string, def) PMap.t;  (** maps loc names to definitions *)
  }

  (** Finds a location name given symbol in the global environment *)
  val find_symbol : t -> string -> (string, err_t) result

  (** Finds a definition given its location name in the global environment *)
  val find_def : t -> string -> def

  (** [set_symbol genv symbol locname ]
      Returns a new global environment where the symbol [symbol] is associated with the location [locname] *)
  val set_symbol : t -> string -> string -> t

  (** [set_def genv locname def ]
      Returns a new global environment where the block [locname] is associated with the global definition [def] *)
  val set_def : t -> string -> def -> t

  (** Empty global environment *)
  val empty : t

  (** Pretty printer for the global environment *)
  val pp : Format.formatter -> t -> unit

  (** {3 Symbolic things} *)

  val substitution : Gillian.Symbolic.Subst.t -> t -> t
  val assertions : t -> string list * Gillian.Gil_syntax.Asrt.t list
end

module Symbolic : sig
  open Gil_syntax

  type nonrec err_t = err_t
  type def = FunDef of Expr.t | GlobVar of Expr.t

  val serialize_def : def -> Expr.t
  val deserialize_def : Expr.t -> def

  type t = {
    symb : (string, string) PMap.t;  (** maps symbols to loc names *)
    defs : (string, def) PMap.t;  (** maps loc names to definitions *)
  }

  (** Finds a location name given symbol in the global environment *)
  val find_symbol : t -> string -> (string, err_t) result

  (** Finds a definition given its location name in the global environment *)
  val find_def : t -> string -> def

  (** [set_symbol genv symbol locname ]
      Returns a new global environment where the symbol [symbol] is associated with the location [locname] *)
  val set_symbol : t -> string -> Gil_syntax.Expr.t -> t Monadic.Delayed.t

  (** [set_def genv locname def ]
      Returns a new global environment where the block [locname] is associated with the global definition [def] *)
  val set_def : t -> string -> def -> t Monadic.Delayed.t

  (** Empty global environment *)
  val empty : t

  (** Pretty printer for the global environment *)
  val pp : Format.formatter -> t -> unit

  (** {3 Symbolic things} *)

  val substitution : Gillian.Symbolic.Subst.t -> t -> t
  val assertions : t -> string list * Gillian.Gil_syntax.Asrt.t list
end
