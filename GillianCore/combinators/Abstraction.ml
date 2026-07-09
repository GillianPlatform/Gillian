open Utils
open Monadic
open Gil_syntax
open Engine
open Engine.Symbolic
module DR = Delayed_result
module L = Logging

type wand = Wands.wand
type upred = Preds.abs_t
type 'a abs = { mem : 'a; preds : upred list; wands : wand list }

let pp_abs pp_state fmt (astate : 'a abs) : unit =
  let { mem; preds; wands; _ } = astate in
  Fmt.pf fmt "%a@\n@[<v 2>PREDICATES:@\n%a@]@\n@[<v 2>WANDS:@\n%a@]@\n" pp_state
    mem
    (Fmt.list ~sep:(Fmt.any "@\n") Preds.pp_pabs)
    preds
    (Fmt.list ~sep:(Fmt.any "@\n") Wands.pp_wand)
    wands

let preds_to_yojson : upred list -> Yojson.Safe.t =
  [%to_yojson: (string * Expr.t list) list]

let preds_of_yojson : Yojson.Safe.t -> (upred list, string) result =
  [%of_yojson: (string * Expr.t list) list]

let wands_to_yojson : wand list -> Yojson.Safe.t = [%to_yojson: Wands.wand list]

let wands_of_yojson : Yojson.Safe.t -> (wand list, string) result =
  [%of_yojson: Wands.wand list]

let abs_to_yojson state_to_yojson (astate : 'a abs) : Yojson.Safe.t =
  (* TODO: Serialize other components of pstate *)
  let { mem; preds; wands; _ } = astate in
  `Assoc
    [
      ("state", state_to_yojson mem);
      ("preds", preds_to_yojson preds);
      ("wands", wands_to_yojson wands);
    ]

let abs_of_yojson state_of_yojson (yojson : Yojson.Safe.t) :
    ('a abs, string) result =
  (* TODO: Deserialize other components of pstate *)
  let open Syntaxes.Result in
  let rec aux = function
    | Some mem, Some preds, Some wands, [] -> Ok { mem; preds; wands }
    | None, preds, wands, ("state", state_yojson) :: rest ->
        let* state = state_of_yojson state_yojson in
        aux (Some state, preds, wands, rest)
    | state, None, wands, ("preds", preds_yojson) :: rest ->
        let* preds = preds_of_yojson preds_yojson in
        aux (state, Some preds, wands, rest)
    | state, preds, None, ("wands", wands_yojson) :: rest ->
        let* wands = wands_of_yojson wands_yojson in
        aux (state, preds, Some wands, rest)
    | _ -> Error "Cannot parse yojson into abstract state"
  in
  match yojson with
  | `Assoc sections -> aux (None, None, None, sections)
  | _ -> Error "Cannot parse yojson into abstract state"

module Make (S : MonadicSMemory.S) = struct
  type t = S.t abs [@@deriving yojson, show]

  (** Errors of the abstraction layer. Each abstraction-specific constructor
      mirrors the [StateErr.t] error that [PState]/[Matcher] raise for the same
      situation today, so that [can_fix]/[get_fixes]/[get_recovery_tactic]/
      [get_failing_constraint] behave identically once predicate reasoning
      happens below the state:
      - [MissingUPred] ≙ [EAsrt (vs, true_)] (the [true_] is what lets the
        recovery mechanism trigger);
      - [MissingWand] ≙ [EPure false_];
      - [AsrtFailure] ≙ [EAsrt];
      - [OtherErr] ≙ [EOther];
      - [SubError] ≙ [EMem]. *)
  type err_t =
    | MissingUPred of { name : string; vs : Expr.t list }
    | MissingWand of { lname : string; rname : string }
    | AsrtFailure of Expr.t list * Expr.t
    | OtherErr of string
    | SubError of S.err_t
  [@@deriving yojson, show]

  type init_data = S.init_data

  let pp_err = pp_err_t
  let sub_err e = SubError e
  let get_init_data s = S.get_init_data s.mem

  let clear (t : t) =
    let pred_defs = MP.get_pred_defs () in
    let preds =
      List.filter
        (fun (p, _) ->
          let def = Hashtbl.find pred_defs p in
          def.pred.pred_pure)
        t.preds
    in
    { wands = []; preds; mem = S.clear t.mem }

  let mem_constraints t = S.mem_constraints t.mem

  type pred =
    | User of string
    | Wand of { lhs : string; rhs : string }
    | SubPred of string
  [@@deriving yojson]

  let is_overlapping_asrt s = S.is_overlapping_asrt s

  type action = Fold | Unfold | GUnfold | Package | SubAction of string

  let action_from_str str =
    if str = SLCmd.fold_action then Fold
    else if str = SLCmd.unfold_action then Unfold
    else if str = SLCmd.gunfold_action then GUnfold
    else if str = SLCmd.package_action then Package
    else SubAction str

  let action_to_str = function
    | Fold -> SLCmd.fold_action
    | Unfold -> SLCmd.unfold_action
    | GUnfold -> SLCmd.gunfold_action
    | Package -> SLCmd.package_action
    | SubAction a -> a

  let pred_from_str str : pred =
    match (Asrt.as_user_pred_name str, Asrt.as_wand_name str) with
    | Some name, _ -> User name
    | _, Some (lhs, rhs) -> Wand { lhs; rhs }
    | _ -> SubPred str

  let pred_to_str pred : string =
    match pred with
    | User name -> Asrt.user_pred_name name
    | Wand { lhs; rhs } -> Asrt.wand_name lhs rhs
    | SubPred p -> p

  (* let empty () : t = { mem = S.empty (); preds = []; wands = [] } *)
  let get_def name = MP.get_pred_def (MP.get_pred_defs ()) name

  let execute_action ~(action_name : string) (s : t) (args : Expr.t list) =
    let open DR.Syntax in
    match action_from_str action_name with
    | SubAction action_name ->
        let++ m', v =
          DR.map_error (S.execute_action ~action_name s.mem args) sub_err
        in
        ({ s with mem = m' }, v)
    | Fold | Unfold | GUnfold | Package -> failwith "todo"

  let consume ~core_pred (s : t) (ins : Expr.t list) :
      (t * Expr.t list, err_t) DR.t =
    let open DR.Syntax in
    match pred_from_str core_pred with
    | SubPred core_pred ->
        let++ mem', outs =
          DR.map_error (S.consume ~core_pred s.mem ins) sub_err
        in
        ({ s with mem = mem' }, outs)
    | _ -> failwith "todo"

  let produce_upred (name : string) (args : Expr.t list) (s : t) =
    let open Delayed.Syntax in
    L.verbose (fun m -> m "Predicate assertion.");
    let pred_def = get_def name in
    let facts =
      (* We're in charge of substituting the facts definition with the arguments of the predicate *)
      let params =
        List.map (fun p -> Expr.PVar (fst p)) pred_def.pred.pred_params
      in
      (* We construct a substituting function *)
      let substitutor =
        List.fold_left2
          (fun acc param arg ->
            fun le ->
             Expr.subst_expr_for_expr ~to_subst:param ~subst_with:arg (acc le))
          Fun.id params args
      in
      List.map substitutor pred_def.pred.pred_facts
    in
    (* Effectively learn the facts *)
    let+ () = Delayed.return ~learned:facts () in
    if pred_def.pred.pred_pure && List.mem (name, args) s.preds then (
      L.verbose (fun fmt ->
          fmt "Pure predicate already there, not producing: %s(%a)" name
            Fmt.(list ~sep:comma Expr.pp)
            args);
      (* Returning the initial state *)
      s)
    else { s with preds = (name, args) :: s.preds }

  let produce_wand lhs rhs args (s : t) =
    let lhs_def = get_def lhs in
    if !Config.under_approximation then L.fail "Wand in under-approx";
    let lhs_nargs = lhs_def.pred.pred_num_params in
    let largs, rargs = List_utils.split_at args lhs_nargs in
    Delayed.return
    @@ {
         s with
         wands = Wands.{ lhs = (lhs, largs); rhs = (rhs, rargs) } :: s.wands;
       }

  let produce ~(core_pred : string) (s : t) (ins_and_outs : Expr.t list) =
    let open Delayed.Syntax in
    match pred_from_str core_pred with
    | User name -> produce_upred name ins_and_outs s
    | Wand { lhs; rhs } -> produce_wand lhs rhs ins_and_outs s
    | SubPred core_pred ->
        let+ sub = S.produce ~core_pred s.mem ins_and_outs in
        { s with mem = sub }

  let is_exclusively_owned (_ : t) (_ : Expr.t list) =
    failwith
      "Cannot determine with certainty if an abstract memory is exclusively \
       owned. Use [Pred_state.is_exclusively_owned] instead."

  let is_empty (_ : t) =
    failwith "Cannot determine with certainty if an abstract memory is empty."

  (* This *)

  let init data = { mem = S.init data; preds = []; wands = [] }

  let wand_to_asrt
      ~pred_defs
      ({ lhs = lname, largs; rhs = rname, rargs } : wand) : Asrt.atom =
    (* The wand's semantic outs are the rhs out-args; its ins are the lhs args
       together with the rhs in-args. *)
    let rpred = MP.get_pred_def pred_defs rname in
    let r_ins = Pred.in_args rpred.pred rargs in
    let r_outs = Pred.out_args rpred.pred rargs in
    Asrt.wand (lname, largs) (rname, r_ins) r_outs

  let upred_to_asrt ~pred_defs ((name, args) : upred) : Asrt.atom =
    let pred_def = MP.get_pred_def pred_defs name in
    let ins = Pred.in_args pred_def.pred args in
    let outs = Pred.out_args pred_def.pred args in
    Asrt.pred name ins outs

  let assertions ?to_keep (s : t) : Asrt.t =
    let pred_defs = MP.get_pred_defs () in
    let sub_asrt = S.assertions ?to_keep s.mem in
    let upred_asrts = List.map (upred_to_asrt ~pred_defs) s.preds in
    let wand_asrts = List.map (wand_to_asrt ~pred_defs) s.wands in
    sub_asrt @ upred_asrts @ wand_asrts

  let can_fix = function
    | MissingUPred _ -> true
    | MissingWand _ -> false
    | AsrtFailure (_, pf) -> Reduction.reduce_lexpr pf <> Expr.false_
    | OtherErr _ -> false
    | SubError e -> S.can_fix e

  let get_fixes = function
    (* User predicates and wands are never abduced (the bi-abduction fix
       machinery refuses such fixes); an empty fix list makes the branch die,
       exactly like [EAsrt (vs, true_)] does today. *)
    | MissingUPred _ | MissingWand _ | OtherErr _ -> []
    | AsrtFailure (_, pf) -> (
        match Reduction.reduce_lexpr pf with
        | Lit (Bool _) -> []
        | pf -> [ [ Asrt.Pure pf ] ])
    | SubError e -> S.get_fixes e

  let get_recovery_tactic (s : t) (e : err_t) =
    match e with
    | MissingUPred { vs; _ } -> Recovery_tactic.try_unfold vs
    | AsrtFailure (vs, _) -> Recovery_tactic.try_unfold vs
    | MissingWand _ | OtherErr _ -> Recovery_tactic.none
    | SubError e -> S.get_recovery_tactic s.mem e

  let lvars (s : t) : Containers.SS.t =
    let open Containers in
    let pred_lvars =
      List.fold_left
        (fun ac (_, vs) ->
          List.fold_left (fun ac e -> SS.union ac (Expr.lvars e)) ac vs)
        SS.empty s.preds
    in
    let wand_lvars =
      let lvars_val_list el =
        List.fold_left
          (fun acc expr -> SS.union acc (Expr.lvars expr))
          SS.empty el
      in
      List.fold_left
        (fun acc ({ lhs = _, largs; rhs = _, rargs } : wand) ->
          acc
          |> SS.union (lvars_val_list largs)
          |> SS.union (lvars_val_list rargs))
        SS.empty s.wands
    in
    S.lvars s.mem |> SS.union pred_lvars |> SS.union wand_lvars

  let alocs (s : t) : Containers.SS.t =
    let open Containers in
    let pred_alocs =
      List.fold_left
        (fun ac (_, vs) ->
          List.fold_left (fun ac e -> SS.union ac (Expr.alocs e)) ac vs)
        SS.empty s.preds
    in
    let wand_alocs =
      let alocs_val_list el =
        List.fold_left (fun acc v -> SS.union acc (Expr.alocs v)) SS.empty el
      in
      List.fold_left
        (fun acc ({ lhs = _, largs; rhs = _, rargs } : wand) ->
          acc
          |> SS.union (alocs_val_list largs)
          |> SS.union (alocs_val_list rargs))
        SS.empty s.wands
    in
    S.alocs s.mem |> SS.union pred_alocs |> SS.union wand_alocs

  let split_further (s : t) (core_pred : string) (ins : Expr.t list) (err : err_t)
      =
    match (pred_from_str core_pred, err) with
    | SubPred core_pred, SubError err ->
        S.split_further s.mem core_pred ins err
    | _ -> None

  let sure_is_nonempty (s : t) =
    S.sure_is_nonempty s.mem
    || (match s.wands with
       | [] -> false
       | _ -> true)
    || List.exists
         (fun (name, _) ->
           let pred_def = MP.get_pred_def (MP.get_pred_defs ()) name in
           not pred_def.pred.pred_pure)
         s.preds
  let pp_by_need vars ft t = pp_abs (S.pp_by_need vars) ft t
  let get_print_info vars s = S.get_print_info vars s.mem

  let substitution (subst : Subst.t) (s : t) =
    let open Delayed.Syntax in
    let subst_in_val (v : Expr.t) : Expr.t =
      Subst.subst_in_expr subst ~partial:true v
    in
    let subst_wand ({ lhs = lname, largs; rhs = rname, rargs } : wand) =
      Wands.
        {
          lhs = (lname, List.map subst_in_val largs);
          rhs = (rname, List.map subst_in_val rargs);
        }
    in
    let subst_upred (s, vs) = (s, List.map subst_in_val vs) in
    let+ mem = S.substitution subst s.mem in
    let preds = List.map subst_upred s.preds in
    let wands = List.map subst_wand s.wands in
    { mem; preds; wands }

  let get_failing_constraint = function
    | MissingUPred _ | MissingWand _ | AsrtFailure _ | OtherErr _ -> Expr.true_
    | SubError e -> S.get_failing_constraint e
end

module _ (S : MonadicSMemory.S) : MonadicSMemory.S with type t = S.t abs =
  Make (S)
