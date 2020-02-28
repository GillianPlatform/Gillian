module LActions : sig
  (** {3 Type definitions} *)

  type mem_ac =
    | Alloc
    | DropPerm
    | GetCurPerm
    | Store
    | Load
    | Free
    | Move
    | MGet
        (** loc -> num (low) -> num (high) -> [ loc; num; num, sval; perm_opt ] *)
    | MSet  (** loc -> num (low) -> num (high) ->  sval -> perm_opt -> [] *)
    | MRem  (** loc -> num (low) -> num (high) -> [] *)

  type genv_ac = GetSymbol | GetDef | SetSymbol | SetDef

  type glob_ac = GetFun | SetFun | RemFun | SetVar

  type ac =
    | AGEnv of genv_ac  (** Actions related to the memory *)
    | AMem  of mem_ac  (** Actions related to the global environment *)
    | AGlob of glob_ac
        (** Actions that are related to both the genv and the memory *)

  type mem_ga = SVal

  type glob_ga = Fun

  type ga = GMem of mem_ga | GGlob of glob_ga

  (** {3 Serialization of actions} *)

  val str_ac : ac -> string
  (** Serializes an action into a string *)

  val ac_from_str : string -> ac
  (** Deserializes a string into an action *)

  (** {3 Global assertion and their actions} *)

  val ga_to_setter : ga -> ac

  val ga_to_getter : ga -> ac

  val ga_to_deleter : ga -> ac

  (** {3 Global assertion serialization } *)

  val str_ga : ga -> string

  val ga_from_str : string -> ga

  val ga_to_setter_str : string -> string

  val ga_to_getter_str : string -> string

  val ga_to_deleter_str : string -> string
end

module GEnv : sig
  open Gillian.Concrete

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
end

module CMemory : Gillian.Concrete.Memory_S

module SMemory : Gillian.Symbolic.Memory_S
