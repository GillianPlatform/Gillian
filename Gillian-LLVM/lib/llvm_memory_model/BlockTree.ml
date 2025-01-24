open SHeapTree
open Gillian
open Gil_syntax
open Gillian.Monadic
module DR = Delayed_result
open SVal

module M = struct
  open LActions

  type err_t = err [@@deriving show, yojson]
  type t = SHeapTree.t [@@deriving show, yojson]
  type action = ac
  type pred = ga

  let action_to_str = str_ac
  let action_from_str s = try Some (ac_from_str s) with _ -> None
  let pred_to_str = str_ga
  let pred_from_str s = try Some (ga_from_str s) with _ -> None

  let list_actions _ =
    [
      (DropPerm, [ "?" ], [ "?" ]);
      (GetCurPerm, [ "?" ], [ "?" ]);
      (WeakValidPointer, [ "?" ], [ "?" ]);
      (Store, [ "?" ], [ "?" ]);
      (Load, [ "?" ], [ "?" ]);
    ]

  let list_preds _ =
    [
      (LActions.Single, [ "?" ], [ "?" ]);
      (LActions.Array, [ "?" ], [ "?" ]);
      (LActions.Hole, [ "?" ], [ "?" ]);
      (LActions.Zeros, [ "?" ], [ "?" ]);
      (LActions.Bounds, [ "?" ], [ "?" ]);
    ]

  let pp_params fmt params =
    let rec aux fmtp = function
      | [] -> ()
      | [ a ] -> Format.fprintf fmt "%a" Expr.pp a
      | a :: r ->
          Format.fprintf fmt "%a, " Expr.pp a;
          aux fmtp r
    in
    Format.fprintf fmt "[%a]" aux params

  let fail_ungracefully act_name params =
    failwith
      (Format.asprintf "Invalid call to %s : %a" act_name pp_params params)

  (** Execute an action *)
  let execute_action (act : action) (s : t) (ins : Expr.t list) :
      (t * Expr.t list, err_t) result Delayed.t =
    let open Delayed.Syntax in
    let open DR.Syntax in
    match (act, ins) with
    | GetCurPerm, [ ofs ] ->
        let** perm = get_perm_at s ofs in
        let perm_string = Expr.Lit (String (Perm.opt_to_string perm)) in
        DR.ok (s, [ perm_string ])
    | WeakValidPointer, [ ofs ] ->
        let** bool = weak_valid_pointer s ofs in
        let res = Expr.bool bool in
        DR.ok (s, [ res ])
    | DropPerm, [ low; high; Expr.Lit (String perm_string) ] ->
        let perm = Perm.of_string perm_string in
        let++ s' = drop_perm s low high perm in
        (s', [])
    | Store, [ Expr.Lit (String chunk_name); ofs; value ] ->
        let chunk = Chunk.of_string chunk_name in
        let sval = SVal.make ~chunk ~value in
        let++ s' = store s chunk ofs sval in
        (s', [])
    | Load, [ Expr.Lit (String chunk_name); ofs ] ->
        let chunk = Chunk.of_string chunk_name in
        let** value, s' = load s chunk ofs in
        let gil_value = SVal.to_gil_expr ~chunk value in
        DR.ok (s', [ gil_value ])
    | _, _ -> fail_ungracefully (action_to_str act) ins

  (** Consume a predicate with the given ins *)
  let consume (pred : pred) (s : t) (ins : Expr.t list) :
      (t * Expr.t list, err_t) result Delayed.t =
    let open Delayed.Syntax in
    let open DR.Syntax in
    match (pred, ins) with
    | Single, [ ofs; Expr.Lit (String chunk_string) ] ->
        let chunk = Chunk.of_string chunk_string in
        let** sval, perm, s' = cons_single s ofs chunk in
        let sval_e = SVal.to_gil_expr ~chunk sval in
        let perm_string = Perm.opt_to_string perm in
        DR.ok (s', [ sval_e; Expr.Lit (String perm_string) ])
    | Array, [ ofs; size; Expr.Lit (String chunk_string) ] ->
        let chunk = Chunk.of_string chunk_string in
        let** array, perm, s' = cons_array s ofs size chunk in
        let array_e = SVArray.to_gil_expr ~size ~chunk array in
        let perm_string = Perm.opt_to_string perm in
        DR.ok (s', [ array_e; Expr.Lit (String perm_string) ])
    | Hole, [ low; high ] ->
        let** s', perm = cons_hole s low high in
        let perm_e = Expr.Lit (String (Perm.opt_to_string perm)) in
        DR.ok (s', [ perm_e ])
    | Zeros, [ low; high ] ->
        let** s', perm = cons_zeros s low high in
        let perm_e = Expr.Lit (String (Perm.opt_to_string perm)) in
        DR.ok (s', [ perm_e ])
    | Bounds, [] ->
        let++ (low, high), s' = cons_bounds s |> DR.of_result in
        let bounds_e = Expr.EList [ low; high ] in
        (s', [ bounds_e ])
    | _, _ -> failwith "Invalid consume call"

  (** Produce a predicate with the given ins and outs *)
  val produce : pred -> t -> Expr.t list -> t Delayed.t

  (** Compose two states together *)
  val compose : t -> t -> t Delayed.t

  (** For Freeable: if a state can be freed. Must only be true if no non-empty state can
   be composed with the state. The Expr list is irrelevant; it's required because of Gillian-C. *)
  val is_exclusively_owned : t -> Expr.t list -> bool Delayed.t

  (** If this state is observably empty. *)
  val is_empty : t -> bool

  (** If this state is entirely made up of concrete expressions. *)
  val is_concrete : t -> bool

  (** Instantiates this state with a list of arguments. This is used by PMap, either in
    static mode with the 'instantiate' action, or in dynamic mode when accessing
    a missing index. *)
  val instantiate : Expr.t list -> t * Expr.t list

  (** The list of core predicates corresponding to the state. *)
  val assertions : t -> (pred * Expr.t list * Expr.t list) list

  (** The list of assertions that aren't core predicates corresponding to the state. *)
  val assertions_others : t -> Asrt.atom list

  (** If the error can be fixed *)
  val can_fix : err_t -> bool

  (** Get the fixes for an error, as a list of fixes -- a fix is a list of core predicates
    to produce onto the state. *)
  val get_fixes : err_t -> pred MyAsrt.t list list

  (** The recovery tactic to attempt to resolve an error, by eg. unfolding predicates *)
  val get_recovery_tactic : err_t -> Expr.t Recovery_tactic.t

  (** The set of logical variables in the state *)
  val lvars : t -> Containers.SS.t

  (** The set of abstract locations in the state *)
  val alocs : t -> Containers.SS.t

  (** Applies a substitution to the state. This can branch, eg. when attempting to resolve
    equality of expressions. *)
  val substitution_in_place : Subst.t -> t -> t Delayed.t

  (** Pretty print the state *)
  val pp : Format.formatter -> t -> unit

  (* Debug *)

  (** (Debug only) Return all available (action * arguments * outputs) *)
  val list_actions : unit -> (action * string list * string list) list

  (** (Debug only) Return all available (predicates * ins * outs) *)
  val list_preds : unit -> (pred * string list * string list) list
end
