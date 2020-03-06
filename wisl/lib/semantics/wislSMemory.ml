open Gillian.Symbolic
open Gillian.Gil_syntax
open Gillian.Logic
module Logging = Gillian.Logging
module SFVL = SFVL
module SS = Gillian.Utils.Containers.SS

type vt = Values.t

type st = Subst.t

type err_t = unit

type c_fix_t = unit

type i_fix_t = unit

type t = WislSHeap.t

type action_ret =
  | ASucc of (t * vt list * Formula.t list * (string * Type.t) list) list
  | AFail of err_t list

let init () = WislSHeap.init ()

let resolve_loc pfs gamma loc =
  Logging.tmi (fun m -> m "get_loc_name: %a" Expr.pp loc);
  let lpfs = PureContext.to_list pfs in
  match Reduction.reduce_lexpr ~pfs ~gamma loc with
  | Expr.Lit (Literal.Loc loc) | Expr.ALoc loc -> Some loc
  | Expr.LVar x -> (
      match Reduction.resolve_expr_to_location lpfs (LVar x) with
      | Some (loc_name, _) -> Some loc_name
      | _                  -> None )
  | loc' -> (
      match Reduction.resolve_expr_to_location lpfs loc' with
      | Some (loc_name, _) -> Some loc_name
      | None               ->
          let msg =
            Format.asprintf "Unsupported location: %a with pfs:\n%a" Expr.pp
              loc' PureContext.pp pfs
          in
          Logging.verboser (fun m -> m "%s" msg);
          raise (Failure msg) )

let get_cell heap pfs gamma (loc : vt) (offset : vt) =
  match resolve_loc pfs gamma loc with
  | Some loc_name -> (
      (* This means loc evaluates to a location, let's look the available offset *)
      match WislSHeap.get_fvl heap loc_name with
      | Some fvl -> (
          (* Location exists in the heap *)
          match SFVL.get offset fvl with
          | Some v ->
              (* The offset is as-is in the heap, with the value, cool ! *)
              ASucc [ (heap, [ loc; offset; v ], [], []) ]
          | None   -> (
              (* The offset is not easy to find in the fvl, we'll try finding a value that is equal
                     according to the path condition and typing env *)
              match
                SFVL.get_first
                  (fun name -> FOSolver.is_equal name offset pfs gamma)
                  fvl
              with
              | Some (o, v) ->
                  (* We found the offset in the heap, modulo some solving, hooray ! *)
                  ASucc [ (heap, [ loc; o; v ], [], []) ]
              | None        ->
                  (* Couldn't find it, we can't do for now. *)
                  AFail [] ) )
      | None     ->
          (* Location does not exist in the heap *)
          AFail [] )
  | None          ->
      (* loc does not evaluate to a location, or we can't find it. *)
      AFail []

let set_cell heap pfs gamma (loc : vt) (offset : vt) (value : vt) =
  let loc_name, new_pfs =
    (* If we can't find the location, we create a new location and we
         add to the path condition that it is equal to the given loc *)
    let resolved_loc_opt = resolve_loc pfs gamma loc in
    match resolved_loc_opt with
    | Some loc_name ->
        if Gillian.Utils.Names.is_aloc_name loc_name then (loc_name, [])
        else (loc_name, [])
    | None          ->
        let al = ALoc.alloc () in
        (al, [ Formula.Eq (Expr.ALoc al, loc) ])
  in
  (* We set the value in the heap.
     If we can find a fvl for that heap, then we modify in the fvl, otherwise,
     we create a new fvl from empty
     This is supposedly correct because, since we got it before, we
     suppose the offset to be correctly found. *)
  let equality_test a b = FOSolver.is_equal a b pfs gamma in
  let () =
    WislSHeap.set_fvl heap loc_name
      (SFVL.add_with_test ~equality_test offset value
         (Option.value ~default:SFVL.empty (WislSHeap.get_fvl heap loc_name)))
  in
  ASucc [ (heap, [], new_pfs, []) ]

let rem_cell heap pfs gamma (loc : vt) (offset : vt) =
  match resolve_loc pfs gamma loc with
  | Some loc_name -> (
      (* This means loc evaluates to a location, let's look the available offset *)
      match WislSHeap.get_fvl heap loc_name with
      | Some fvl ->
          (* Location exists in the heap *)
          let new_fvl = SFVL.remove offset fvl in
          let () = WislSHeap.set_fvl heap loc_name new_fvl in
          let () = ALoc.dealloc loc_name in
          ASucc [ (heap, [], [], []) ]
      | None     ->
          (* Location does not exist in the heap *)
          AFail [] )
  | None          ->
      (* loc does not evaluate to a location, or we can't find it. *)
      AFail []

let alloc heap _pfs _gamma (size : int) =
  let loc = WislSHeap.alloc heap size in
  ASucc
    [ (heap, [ Expr.Lit (Literal.Loc loc); Expr.Lit (Literal.Int 0) ], [], []) ]

let dispose heap pfs gamma loc_expr =
  match resolve_loc pfs gamma loc_expr with
  | Some loc_name -> (
      match WislSHeap.get_fvl heap loc_name with
      | Some fvl ->
          let () = WislSHeap.remove heap loc_name in
          ASucc [ (heap, [], [], []) ]
      | None     ->
          Logging.verboser (fun m ->
              m
                "!!!!!!!!!!!!!!!@\n\
                 %s is not in the memory, can't dispose of it !!!@\n\
                 !!!!!!!!!!!"
                loc_name);
          AFail [] )
  | None          -> AFail []

let execute_action name heap pfs gamma args =
  let action = WislLActions.ac_from_str name in
  match action with
  | GetCell -> (
      match args with
      | [ loc_expr; offset_expr ] ->
          get_cell heap pfs gamma loc_expr offset_expr
      | args                      ->
          failwith
            (Format.asprintf
               "Invalid GetCell Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args) )
  | SetCell -> (
      match args with
      | [ loc_expr; offset_expr; value_expr ] ->
          set_cell heap pfs gamma loc_expr offset_expr value_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid SetCell Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args) )
  | RemCell -> (
      match args with
      | [ loc_expr; offset_expr ] ->
          rem_cell heap pfs gamma loc_expr offset_expr
      | args                      ->
          failwith
            (Format.asprintf
               "Invalid RemCell Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args) )
  | Alloc   -> (
      match args with
      | [ Expr.Lit (Literal.Int size) ] when size >= 1 ->
          alloc heap pfs gamma size
      | args ->
          failwith
            (Format.asprintf
               "Invalid Alloc Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args) )
  | Dispose -> (
      match args with
      | [ loc_expr ] -> dispose heap pfs gamma loc_expr
      | args         ->
          failwith
            (Format.asprintf
               "Invalid Dispose Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args) )

let ga_to_setter = WislLActions.ga_to_setter_str

let ga_to_getter = WislLActions.ga_to_getter_str

let ga_to_deleter = WislLActions.ga_to_deleter_str

let ga_loc_indexes a_id =
  WislLActions.(
    match ga_from_str a_id with
    | Cell -> [ 0 ])

let copy = WislSHeap.copy

let pp fmt h = Format.fprintf fmt "%s" (WislSHeap.str h)

let pp_err _ _ = ()

let get_recovery_vals _ = []

let pp_c_fix _ _ = ()

let pp_i_fix _ _ = ()

let substitution_in_place = WislSHeap.substitution_in_place

let fresh_val _ = Expr.LVar (LVar.alloc ())

let clean_up _ = ()

(** FIXME: that's not normal ? *)
let lvars _heap = SS.empty

let assertions ?to_keep:_ heap = WislSHeap.assertions heap

let mem_constraints _ = []

let is_overlapping_asrt _ = false

let apply_fix m _ _ _ = m

let get_fixes ?simple_fix _ _ _ _ = []

let get_failing_constraint _ = Formula.True
