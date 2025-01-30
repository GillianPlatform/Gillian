open SHeapTree
open Gillian
open Gil_syntax
open Gillian.Monadic
module DR = Delayed_result
open SVal
module Subst = Gillian.Symbolic.Subst
open States

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
        (* TODO(Ian): This is unsound we are making an SVal with an arbitrary chunk*)
        let sval = SVal.make ~chunk ~value in
        let++ s' = store s chunk ofs sval in
        (s', [])
    | Load, [ Expr.Lit (String chunk_name); ofs ] ->
        let chunk = Chunk.of_string chunk_name in
        Logging.tmi (fun m -> m "Loading");
        let** value, s' = load s chunk ofs in
        let+ gil_value = SVal.to_gil_expr ~chunk value in
        Ok (s', [ gil_value ])
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
        let+ sval_e = SVal.to_gil_expr ~chunk sval in
        let perm_string = Perm.opt_to_string perm in
        Ok (s', [ sval_e; Expr.Lit (String perm_string) ])
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
        let++ bounds, s' = cons_bounds s |> DR.of_result in
        let bounds_e =
          match bounds with
          | None -> Expr.Lit Null
          | Some (low, high) -> Expr.EList [ low; high ]
        in
        (s', [ bounds_e ])
    | _, _ -> failwith "Invalid consume call"

  (** Produce a predicate with the given ins and outs *)
  let produce (pred : pred) (s : t) (insouts : Expr.t list) : t Delayed.t =
    let open Delayed.Syntax in
    let open DR.Syntax in
    let open Delayed.Syntax in
    let filter_errors dr =
      Delayed.bind dr (fun res ->
          match res with
          | Ok res -> Delayed.return res
          | Error err ->
              Logging.tmi (fun m -> m "Filtering error branch: %a" pp_err err);
              Delayed.vanish ())
    in
    match (pred, insouts) with
    | ( Single,
        [
          ofs;
          Expr.Lit (String chunk_string);
          sval_e;
          Expr.Lit (String perm_string);
        ] ) ->
        let perm = Perm.of_string perm_string in
        let chunk = Chunk.of_string chunk_string in
        let sval = SVal.make ~chunk ~value:sval_e in
        prod_single s ofs chunk sval perm |> filter_errors
    | ( Array,
        [
          ofs;
          size;
          Expr.Lit (String chunk_string);
          arr_e;
          Expr.Lit (String perm_string);
        ] ) ->
        let perm = Perm.of_string perm_string in
        let chunk = Chunk.of_string chunk_string in
        let arr = SVArray.make ~chunk ~values:arr_e in
        prod_array s ofs size chunk arr perm |> filter_errors
    | Hole, [ low; high; Expr.Lit (String perm_string) ] ->
        let perm = Perm.of_string perm_string in
        prod_hole s low high perm |> filter_errors
    | Zeros, [ low; high; Expr.Lit (String perm_string) ] ->
        let perm = Perm.of_string perm_string in
        prod_zeros s low high perm |> filter_errors
    | Bounds, [ bounds_e ] ->
        let bounds =
          match bounds_e with
          | Expr.EList [ low; high ] -> (low, high)
          | Lit (LList [ low; high ]) -> (Lit low, Lit high)
          | _ -> failwith "set_bounds wrong param"
        in
        prod_bounds s bounds |> DR.of_result |> filter_errors
    | _, _ -> failwith "Invalid produce call"

  (** Compose two states together *)
  let compose s1 s2 =
    let open Delayed.Syntax in
    let* res = merge ~old_tree:s1 ~new_tree:s2 in
    match res with
    | Ok s' -> Delayed.return s'
    | Error e ->
        Logging.verbose (fun fmt ->
            fmt "Vanishing on compose error: %a" pp_err_t e);
        Delayed.vanish ()

  (** For Freeable: if a state can be freed. Must only be true if no non-empty state can
   be composed with the state. The Expr list is irrelevant; it's required because of Gillian-C. *)
  let is_exclusively_owned tree e =
    let open Delayed.Syntax in
    match e with
    | [ low; high ] -> SHeapTree.is_exclusively_owned tree low high
    | _ -> Delayed.return false

  let empty _ = SHeapTree.empty

  (** If this state is observably empty. *)
  let is_empty = SHeapTree.is_empty

  (** If this state is entirely made up of concrete expressions. *)
  let is_concrete = SHeapTree.is_concrete

  (** Instantiates this state with a list of arguments. This is used by PMap, either in
    static mode with the 'instantiate' action, or in dynamic mode when accessing
    a missing index. *)
  let instantiate = function
    | [ low; high ] ->
        let tree = SHeapTree.instantiate low high in
        (tree, [])
    | _ -> failwith "BlockTree: Invalid instantiate arguments"

  (** The list of core predicates corresponding to the state. *)
  let assertions tree = SHeapTree.assertions tree

  (** The list of assertions that aren't core predicates corresponding to the state. *)
  let assertions_others tree : Asrt.atom list = SHeapTree.assertions_others tree

  (** If the error can be fixed *)
  let can_fix (e : err_t) =
    match e with
    | MissingResource _ -> true
    | _ -> false

  (** Get the fixes for an error, as a list of fixes -- a fix is a list of core predicates
    to produce onto the state. *)
  let get_fixes e =
    Logging.tmi (fun m -> m "Getting fixes for %a" pp_err e);
    match e with
    | MissingResource (Fixable { is_store; low = ofs; chunk }) ->
        Logging.tmi (fun m -> m "Fixable");
        let freeable_perm = Perm.to_string Perm.Freeable |> Expr.string in
        let chunk_as_expr = Chunk.to_string chunk |> Expr.string in
        let new_var1 = Expr.LVar (LVar.alloc ()) in
        [
          [
            MyAsrt.CorePred
              (Single, [ ofs; chunk_as_expr ], [ new_var1; freeable_perm ]);
          ];
        ]
    | _ -> []

  (** The recovery tactic to attempt to resolve an error, by eg. unfolding predicates *)
  let get_recovery_tactic _ = Gillian.General.Recovery_tactic.none

  (** The set of logical variables in the state *)
  let lvars tree = SHeapTree.lvars tree

  (** The set of abstract locations in the state *)
  let alocs tree = SHeapTree.alocs tree

  (** Applies a substitution to the state. This can branch, eg. when attempting to resolve
    equality of expressions. *)
  let substitution_in_place subst tree =
    let le_subst = Subst.subst_in_expr subst ~partial:true in
    let sval_subst = SVal.substitution ~le_subst in
    let svarr_subst = SVArray.subst ~le_subst in
    substitution ~le_subst ~sval_subst ~svarr_subst tree |> Delayed.return

  let move
      (dst_tree : t)
      (dst_ofs : Expr.t)
      (src_tree : t)
      (src_ofs : Expr.t)
      (size : Expr.t) =
    SHeapTree.move dst_tree dst_ofs src_tree src_ofs size

  (** Pretty print the state *)
  let pp fmt tree = SHeapTree.pp_full fmt tree

  (* Debug *)

  (** (Debug only) Return all available (action * arguments * outputs) *)
  let list_actions _ =
    [
      (DropPerm, [ "?" ], [ "?" ]);
      (GetCurPerm, [ "?" ], [ "?" ]);
      (WeakValidPointer, [ "?" ], [ "?" ]);
      (Store, [ "?" ], [ "?" ]);
      (Load, [ "?" ], [ "?" ]);
    ]

  (** (Debug only) Return all available (predicates * ins * outs) *)
  let list_preds _ =
    [
      (Single, [ "?" ], [ "?" ]);
      (Array, [ "?" ], [ "?" ]);
      (Hole, [ "?" ], [ "?" ]);
      (Zeros, [ "?" ], [ "?" ]);
      (Bounds, [ "?" ], [ "?" ]);
    ]
end
