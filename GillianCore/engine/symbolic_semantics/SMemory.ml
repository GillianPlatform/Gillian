module type S = sig
  (** Type of data that is given the first time memory is created.
      Useful when there's global context to know about like a type-system *)
  type init_data

  (** Type of GIL values *)
  type vt = SVal.M.t

  (** Type of GIL substitutions *)
  type st = SVal.SESubst.t

  type err_t [@@deriving yojson, show]

  (** Type of GIL general states *)
  type t [@@deriving yojson]

  (** Initialisation *)
  val init : init_data -> t

  val get_init_data : t -> init_data
  val clear : t -> t

  (** Execute action *)
  val execute_action :
    string -> t -> Gpc.t -> vt list -> (t * vt list, err_t) Symex.result

  (* Consumers have the same signature as action executors,
     but take a core-predicate name as parameter instead of action name.
     Theoretically, errors for consumers are different: they're logical errors or missing errors,
     as opposed to language error and missing errors. *)
  val consume :
    string -> t -> Gpc.t -> vt list -> (t * vt list, err_t) Symex.result

  (* Producers cannot fail *)
  val produce : string -> t -> Gpc.t -> vt list -> t Symex.t
  val is_overlapping_asrt : string -> bool

  (** State Copy *)
  val copy : t -> t

  (** Printer *)
  val pp : Format.formatter -> t -> unit

  val pp_by_need : Containers.SS.t -> Format.formatter -> t -> unit
  val get_print_info : Containers.SS.t -> t -> Containers.SS.t * Containers.SS.t

  val substitution_in_place :
    pfs:PFS.t ->
    gamma:Type_env.t ->
    st ->
    t ->
    (t * Formula.Set.t * (string * Type.t) list) list

  val clean_up : ?keep:Expr.Set.t -> t -> Expr.Set.t * Expr.Set.t
  val lvars : t -> Containers.SS.t
  val alocs : t -> Containers.SS.t
  val assertions : ?to_keep:Containers.SS.t -> t -> Asrt.t list
  val mem_constraints : t -> Formula.t list
  val get_recovery_tactic : t -> err_t -> vt Recovery_tactic.t
  val pp_err : Format.formatter -> err_t -> unit
  val get_failing_constraint : err_t -> Formula.t
  val get_fixes : err_t -> Asrt.t list list
  val can_fix : err_t -> bool
  val sure_is_nonempty : t -> bool

  (** [split_further core_pred ins err] returns a way to split further a core_predicate if consuming it failed with error, if there is one.
      In that case, it returns a pair containing
      - a list of new ins. Each element is the list of ins for each sub-component of the core predicate;
      - new way of learning the outs, as explained under.

      For example let's say the core predicate [(x, []) ↦ [a, b]] (with 2 ins and 1 out) can be split into
      - [(x, [0]) ↦ [a]]
      - [(x, [1]) ↦ [b]]
      And we try and consume the whole thing, but the memory only had [(x, [0]) ↦ [a]] in it.
      Then this function, given the appropriate error, should a pair of two elements:
      - the new ins: [ [ [x, [0]], [x, [1]] ] ]
      - the new way of learning the outs: [ [  {{ l-nth(PVar("0:0"), 0), l-nth(PVar("1:0"), 0) }}   ] ]

      {b Important}: it is always sound for this function to return [None], it will just reduce the amount of automation.
      *)
  val split_further :
    t -> string -> vt list -> err_t -> (vt list list * vt list) option
end

module Dummy : S with type init_data = unit = struct
  type init_data = unit
  type vt = SVal.M.t
  type st = SVal.SESubst.t
  type err_t = unit [@@deriving yojson, show]
  type t = unit [@@deriving yojson]

  let init () = ()
  let get_init_data () = ()
  let clear () = ()
  let execute_action _ _ _ _ = failwith "Please implement SMemory"
  let consume _ _ _ _ = failwith "Please implement SMemory"
  let produce _ _ _ _ = failwith "Please implement SMemory"
  let is_overlapping_asrt _ = failwith "Please implement SMemory"
  let copy () = ()
  let pp _ _ = ()
  let pp_by_need _ _ _ = ()
  let get_print_info _ _ = failwith "Please implement SMemory"
  let substitution_in_place ~pfs:_ ~gamma:_ _ _ = []
  let clean_up ?keep:_ _ = failwith "Please implement SMemory"
  let lvars _ = failwith "Please implement SMemory"
  let alocs _ = failwith "Please implement SMemory"
  let assertions ?to_keep:_ _ = failwith "Please implement SMemory"
  let mem_constraints _ = failwith "Please implement SMemory"
  let get_recovery_tactic _ _ = failwith "Please implement SMemory"
  let pp_err _ _ = ()
  let get_failing_constraint _ = failwith "Please implement SMemory"
  let get_fixes _ = failwith "Please implement SMemory"
  let can_fix _ = failwith "Please implement SMemory"
  let sure_is_nonempty _ = failwith "Please implement SMemory"
  let split_further _ _ _ = failwith "Please implement SMemory"
end
