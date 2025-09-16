open Gillian.Symbolic
open Gillian.Gil_syntax
open Gillian.Utils
open Gillian.Monadic
open Delayed.Syntax
open Delayed_result
open Delayed_result.Syntax
module Recovery_tactic = Gillian.General.Recovery_tactic
module Logging = Gillian.Logging
module SFVL = SFVL
module SS = Gillian.Utils.Containers.SS

type init_data = unit
type vt = Values.t
type err_t = WislSHeap.err [@@deriving yojson, show]
type t = WislSHeap.t [@@deriving yojson]

let init () = WislSHeap.init ()
let get_init_data _ = ()
let clear _ = WislSHeap.init ()

let resolve_loc loc =
  let* loc_opt = Delayed.resolve_loc loc in
  match loc_opt with
  | None -> error (WislSHeap.InvalidLocation loc)
  | Some loc -> ok loc

let resolve_loc_or_alloc loc =
  let* resolved_loc_opt = Delayed.resolve_loc loc in
  (* If we can't find the location, we create a new location and we
       add to the path condition that it is equal to the given loc *)
  match resolved_loc_opt with
  | Some loc_name -> Delayed.return loc_name
  | None ->
      let al = ALoc.alloc () in
      Delayed.return ~learned:[ BinOp (ALoc al, Equal, loc) ] al

let get_cell heap (loc : vt) (offset : vt) =
  let** loc = resolve_loc loc in
  let++ loc, ofs, value = WislSHeap.get_cell heap loc offset in
  let loc = Expr.loc_from_loc_name loc in
  (heap, [ loc; ofs; value ])

let set_cell heap (loc : vt) (offset : vt) (value : vt) =
  let* loc = resolve_loc_or_alloc loc in
  let++ () = WislSHeap.set_cell heap loc offset value in
  (heap, [])

let rem_cell heap (loc : vt) (offset : vt) =
  let** loc = resolve_loc loc in
  let++ () = WislSHeap.rem_cell heap loc offset in
  (heap, [])

let get_bound heap loc =
  let** loc = resolve_loc loc in
  let++ b = WislSHeap.get_bound heap loc in
  let b = Expr.int b in
  let loc = Expr.loc_from_loc_name loc in
  (heap, [ loc; b ])

let set_bound heap (loc : vt) (bound : int) =
  let* loc = resolve_loc_or_alloc loc in
  let++ () = WislSHeap.set_bound heap loc bound in
  (heap, [])

let rem_bound heap loc =
  let** loc = resolve_loc loc in
  let++ () = WislSHeap.rem_bound heap loc in
  (heap, [])

let get_freed heap loc =
  let** loc = resolve_loc loc in
  let++ () = WislSHeap.get_freed heap loc in
  let loc = Expr.loc_from_loc_name loc in
  (heap, [ loc ])

let set_freed heap (loc : vt) =
  let* loc = resolve_loc_or_alloc loc in
  let+ () = WislSHeap.set_freed heap loc in
  Ok (heap, [])

let rem_freed heap loc =
  let** loc = resolve_loc loc in
  let++ () = WislSHeap.rem_freed heap loc in
  (heap, [])

let alloc heap (size : int) =
  let loc = WislSHeap.alloc heap size in
  ok (heap, [ Expr.Lit (Loc loc); Lit (Int Z.zero) ])

let dispose heap loc =
  let** loc = resolve_loc loc in
  let++ () = WislSHeap.dispose heap loc in
  (heap, [])

let execute_action ~action_name heap args =
  let action = WislLActions.ac_from_str action_name in
  match action with
  | GetCell -> (
      match args with
      | [ loc_expr; offset_expr ] -> get_cell heap loc_expr offset_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid GetCell Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | SetCell -> (
      match args with
      | [ loc_expr; offset_expr; value_expr ] ->
          set_cell heap loc_expr offset_expr value_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid SetCell Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | RemCell -> (
      match args with
      | [ loc_expr; offset_expr ] -> rem_cell heap loc_expr offset_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid RemCell Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | GetBound -> (
      match args with
      | [ loc_expr ] -> get_bound heap loc_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid GetBound Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | SetBound -> (
      match args with
      | [ loc_expr; Expr.Lit (Int b) ] -> set_bound heap loc_expr (Z.to_int b)
      | args ->
          failwith
            (Format.asprintf
               "Invalid SetBound Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | RemBound -> (
      match args with
      | [ loc_expr ] -> rem_bound heap loc_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid RemBound Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | GetFreed -> (
      match args with
      | [ loc_expr ] -> get_freed heap loc_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid GetFreed Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | SetFreed -> (
      match args with
      | [ loc_expr ] -> set_freed heap loc_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid SetFreed Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | RemFreed -> (
      match args with
      | [ loc_expr ] -> rem_freed heap loc_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid RemFreed Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | Alloc -> (
      match args with
      | [ Expr.Lit (Literal.Int size) ] when Z.geq size Z.one ->
          alloc heap (Z.to_int size)
      | args ->
          failwith
            (Format.asprintf
               "Invalid Alloc Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | Dispose -> (
      match args with
      | [ loc_expr ] -> dispose heap loc_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid Dispose Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))

let ga_to_setter = WislLActions.ga_to_setter_str
let ga_to_getter = WislLActions.ga_to_getter_str
let ga_to_deleter = WislLActions.ga_to_deleter_str
let copy = WislSHeap.copy
let pp fmt h = Format.fprintf fmt "%a" WislSHeap.pp h

(* TODO: Implement properly *)
let pp_by_need _ fmt h = pp fmt h

(* TODO: Implement properly *)
let get_print_info _ _ = (SS.empty, SS.empty)

let pp_err fmt t =
  match t with
  | WislSHeap.MissingResource _ -> Fmt.pf fmt "Missing Resource"
  | DoubleFree _ -> Fmt.pf fmt "Double Free"
  | UseAfterFree _ -> Fmt.pf fmt "Use After Free"
  | MemoryLeak -> Fmt.pf fmt "Memory Leak"
  | OutOfBounds _ -> Fmt.pf fmt "Out Of Bounds"
  | InvalidLocation loc ->
      Fmt.pf fmt "Invalid Location: '%a' cannot be resolved as a location"
        Expr.pp loc

let get_recovery_tactic _ e =
  match e with
  | WislSHeap.MissingResource (_, loc, ofs) ->
      let loc = Expr.loc_from_loc_name loc in
      let ofs = Option.to_list ofs in
      Recovery_tactic.try_unfold (loc :: ofs)
  | _ -> Recovery_tactic.none

let substitution_in_place = WislSHeap.substitution_in_place

let clean_up ?(keep = Expr.Set.empty) (mem : t) : Expr.Set.t * Expr.Set.t =
  WislSHeap.clean_up keep mem

let lvars heap = WislSHeap.lvars heap
let alocs heap = WislSHeap.alocs heap
let assertions ?to_keep:_ heap = WislSHeap.assertions heap
let mem_constraints _ = []
let is_overlapping_asrt _ = false

let get_fixes (err : err_t) =
  Logging.verbose (fun m -> m "Getting fixes for error : %a" pp_err err);
  match err with
  | MissingResource (Cell, loc, Some ofs) ->
      let new_var = LVar.alloc () in
      let value = Expr.LVar new_var in
      let loc = Expr.loc_from_loc_name loc in
      let ga = WislLActions.str_ga WislLActions.Cell in
      [ [ Asrt.CorePred (ga, [ loc; ofs ], [ value ]) ] ]
  | InvalidLocation loc ->
      let new_loc = ALoc.alloc () in
      let new_expr = Expr.ALoc new_loc in
      [ [ Asrt.Pure (BinOp (new_expr, Equal, loc)) ] ]
  | _ -> []

let can_fix = function
  | WislSHeap.InvalidLocation _ | MissingResource _ -> true
  | _ -> false

let get_failing_constraint _ = Expr.true_
let sure_is_nonempty t = not (WislSHeap.is_empty t)

let consume ~core_pred heap args =
  let getter = ga_to_getter core_pred in
  let deleter = ga_to_deleter core_pred in
  let** heap', vs = execute_action ~action_name:getter heap args in
  let vs_ins, vs_outs = List_utils.split_at vs (List.length args) in
  let++ heap'', _ = execute_action ~action_name:deleter heap' vs_ins in
  (heap'', vs_outs)

let produce ~core_pred heap args =
  let setter = ga_to_setter core_pred in
  let* set_res = execute_action ~action_name:setter heap args in
  match set_res with
  | Error _ ->
      (* It's ok for failing producers to vanish, no unsoundness *)
      Delayed.vanish ()
  | Ok (heap', _) -> Delayed.return heap'

let split_further _ _ _ _ = None
