module type S = sig
  (** Type of data that is given the first time memory is created.
      Useful when there's global context to know about like a type-system *)
  type init_data

  (** Type of GIL values *)
  type vt = SVal.M.t

  (** Type of GIL substitutions *)
  type st = SVal.SESubst.t

  type c_fix_t
  type err_t [@@deriving yojson, show]

  (** Type of GIL general states *)
  type t [@@deriving yojson]

  type action_ret =
    ( (t * vt list * Formula.t list * (string * Type.t) list) list,
      err_t list )
    result

  (** Initialisation *)
  val init : init_data -> t

  val clear : t -> t

  (** Execute action *)
  val execute_action :
    ?unification:bool ->
    string ->
    t ->
    PFS.t ->
    Type_env.t ->
    vt list ->
    action_ret

  val ga_to_setter : string -> string
  val ga_to_getter : string -> string
  val ga_to_deleter : string -> string
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

  val fresh_val : t -> vt
  val clean_up : ?keep:Expr.Set.t -> t -> Expr.Set.t * Expr.Set.t
  val lvars : t -> Containers.SS.t
  val alocs : t -> Containers.SS.t
  val assertions : ?to_keep:Containers.SS.t -> t -> Asrt.t list
  val mem_constraints : t -> Formula.t list
  val pp_c_fix : Format.formatter -> c_fix_t -> unit
  val get_recovery_tactic : t -> err_t -> vt Recovery_tactic.t
  val pp_err : Format.formatter -> err_t -> unit
  val get_failing_constraint : err_t -> Formula.t

  val get_fixes :
    t ->
    PFS.t ->
    Type_env.t ->
    err_t ->
    (c_fix_t list * Formula.t list * (string * Type.t) list * Containers.SS.t)
    list

  val apply_fix : t -> PFS.t -> Type_env.t -> c_fix_t -> t list
end

module Dummy : S with type init_data = unit = struct
  type init_data = unit
  type vt = SVal.M.t
  type st = SVal.SESubst.t
  type c_fix_t = unit
  type err_t = unit [@@deriving yojson, show]
  type t = unit [@@deriving yojson]

  type action_ret =
    ( (t * vt list * Formula.t list * (string * Type.t) list) list,
      err_t list )
    result

  let init () = ()
  let clear () = ()

  let execute_action ?unification:_ _ _ _ _ _ =
    failwith "Please implement SMemory"

  let ga_to_setter _ = failwith "Please implement SMemory"
  let ga_to_getter _ = failwith "Please implement SMemory"
  let ga_to_deleter _ = failwith "Please implement SMemory"
  let is_overlapping_asrt _ = failwith "Please implement SMemory"
  let copy () = ()
  let pp _ _ = ()
  let pp_by_need _ _ _ = ()
  let get_print_info _ _ = failwith "Please implement SMemory"
  let substitution_in_place ~pfs:_ ~gamma:_ _ _ = []
  let fresh_val _ = failwith "Please implement SMemory"
  let clean_up ?keep:_ _ = failwith "Please implement SMemory"
  let lvars _ = failwith "Please implement SMemory"
  let alocs _ = failwith "Please implement SMemory"
  let assertions ?to_keep:_ _ = failwith "Please implement SMemory"
  let mem_constraints _ = failwith "Please implement SMemory"
  let pp_c_fix _ _ = ()
  let get_recovery_tactic _ _ = failwith "Please implement SMemory"
  let pp_err _ _ = ()
  let get_failing_constraint _ = failwith "Please implement SMemory"
  let get_fixes _ _ _ _ = failwith "Please implement SMemory"
  let apply_fix _ _ _ _ = failwith "Please implement SMemory"
end

module Modernize (Old_memory : S) = struct
  include Old_memory

  let execute_action action_name heap (pc : Gpc.t) args =
    let open Syntaxes.List in
    match
      execute_action ~unification:pc.unification action_name heap pc.pfs
        pc.gamma args
    with
    | Ok oks ->
        let+ new_heap, v, new_fofs, new_types = oks in
        let new_pfs = PFS.copy pc.pfs in
        let new_gamma = Type_env.copy pc.gamma in
        List.iter (fun (x, t) -> Type_env.update new_gamma x t) new_types;
        List.iter (fun fof -> PFS.extend new_pfs fof) new_fofs;
        let new_pc =
          Gpc.make ~unification:pc.unification ~pfs:new_pfs ~gamma:new_gamma ()
        in
        Gbranch.{ pc = new_pc; value = Ok (new_heap, v) }
    | Error errs ->
        let+ err = errs in
        let pc = Gpc.copy pc in
        Gbranch.{ pc; value = Error err }

  let apply_fix heap pfs gamma fix =
    let open Syntaxes.List in
    let+ heap = apply_fix heap pfs gamma fix in
    let pc = Gpc.make ~unification:true ~pfs ~gamma () in
    Gbranch.{ pc; value = heap }
end
