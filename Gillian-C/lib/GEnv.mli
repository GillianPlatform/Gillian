open Gillian.Concrete

(** {2 Init Data} *)

type init_data =
  | Init_int8    of int
  | Init_int16   of int
  | Init_int32   of int
  | Init_int64   of int
  | Init_float32 of float
  | Init_float64 of float
  | Init_space   of int
  | Init_addrof  of string * int

val init_data_size : init_data -> int

val init_data_of_gil : Gil_syntax.Literal.t -> init_data

type def = FunDef of string | GlobVar of string

val serialize_def : def -> Values.t

val deserialize_def : Values.t -> def

type t = {
  symb : (string, string) Gillian.Utils.PMap.t;
      (** maps symbols to loc names *)
  defs : (string, def) Gillian.Utils.PMap.t;
      (** maps loc names to definitions *)
}

val find_symbol : t -> string -> string
(** Finds a location name given symbol in the global environment *)

val find_def : t -> string -> def
(** Finds a definition given its location name in the global environment *)

val set_symbol : t -> string -> string -> t
(** [set_symbol genv symbol locname ]
    Returns a new global environment where the symbol [symbol] is associated with the location [locname] *)

val set_def : t -> string -> def -> t
(** [set_def genv locname def ]
    Returns a new global environment where the block [locname] is associated with the global definition [def] *)

val find_def_from_symbol : t -> string -> def
(** [find_def_from_symbol genv symbol] is the equivalent of applying first find_symbol and then find_def *)

val rem_symbol_and_def : t -> string -> t
(** [rem_symbol_and_def genv sym] removes the symbol from the symbol table
      AND the associated location from the definition table *)

val empty : t
(** Empty global environment *)

val pp : Format.formatter -> t -> unit
(** Pretty printer for the global environment *)

(** {3 Symbolic things} *)

val substitution : Gillian.Symbolic.Subst.t -> t -> t

val assertions : t -> string list * Gillian.Gil_syntax.Asrt.t list
