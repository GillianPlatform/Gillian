open Gillian.Symbolic
open Gillian.Gil_syntax
open Gillian.Logic
module Logging = Gillian.Logging
module SFVL = SFVL
module SS = Gillian.Utils.Containers.SS

type init_data = unit
type vt = Values.t
type st = Subst.t
type err_t = WislSHeap.Err.t [@@deriving yojson, show]
type c_fix_t = unit
type t = WislSHeap.t [@@deriving yojson]

type action_ret =
  | ASucc of (t * vt list * Formula.t list * (string * Type.t) list) list
  | AFail of err_t list

let init () = WislSHeap.init ()
let clear _ = WislSHeap.init ()

let with_resolved_loc ~pfs ~gamma loc f =
  match FOSolver.resolve_loc_name ~pfs ~gamma loc with
  | Some loc_name -> f loc_name
  | None -> AFail [ InvalidLocation loc ]

let resolve_or_create_loc ~pfs ~gamma loc =
  (* If we can't find the location, we create a new location and we
      add to the path condition that it is equal to the given loc *)
  match FOSolver.resolve_loc_name ~pfs ~gamma loc with
  | Some loc_name -> (loc_name, [])
  | None ->
      let al = ALoc.alloc () in
      (al, [ Formula.Eq (Expr.ALoc al, loc) ])

let get_cell heap pfs gamma (loc : vt) (offset : vt) =
  with_resolved_loc ~pfs ~gamma loc @@ fun loc ->
  match WislSHeap.get_cell ~pfs ~gamma heap loc offset with
  | Error err -> AFail [ err ]
  | Ok (loc, ofs, value) ->
      let loc = Expr.loc_from_loc_name loc in
      ASucc [ (heap, [ loc; ofs; value ], [], []) ]

let set_cell heap pfs gamma (loc : vt) (offset : vt) (value : vt) =
  let loc_name, new_pfs = resolve_or_create_loc ~pfs ~gamma loc in
  match WislSHeap.set_cell ~pfs ~gamma heap loc_name offset value with
  | Error e -> AFail [ e ]
  | Ok () -> ASucc [ (heap, [], new_pfs, []) ]

let rem_cell heap pfs gamma (loc : vt) (offset : vt) =
  with_resolved_loc ~pfs ~gamma loc @@ fun loc_name ->
  match WislSHeap.rem_cell heap loc_name offset with
  | Error e -> AFail [ e ]
  | Ok () -> ASucc [ (heap, [], [], []) ]

let get_bound heap pfs gamma loc =
  with_resolved_loc ~pfs ~gamma loc @@ fun loc_name ->
  match WislSHeap.get_bound heap loc_name with
  | Error e -> AFail [ e ]
  | Ok b ->
      let b = Expr.int b in
      let loc = Expr.loc_from_loc_name loc_name in
      ASucc [ (heap, [ loc; b ], [], []) ]

let set_bound heap pfs gamma (loc : vt) (bound : int) =
  let loc_name, new_pfs = resolve_or_create_loc ~pfs ~gamma loc in
  match WislSHeap.set_bound heap loc_name bound with
  | Error e -> AFail [ e ]
  | Ok () -> ASucc [ (heap, [], new_pfs, []) ]

let rem_bound heap pfs gamma loc =
  with_resolved_loc ~pfs ~gamma loc @@ fun loc_name ->
  match WislSHeap.rem_bound heap loc_name with
  | Error e -> AFail [ e ]
  | Ok () -> ASucc [ (heap, [], [], []) ]

let get_freed heap pfs gamma loc =
  with_resolved_loc ~pfs ~gamma loc @@ fun loc_name ->
  match WislSHeap.get_freed heap loc_name with
  | Error e -> AFail [ e ]
  | Ok () ->
      let loc = Expr.loc_from_loc_name loc_name in
      ASucc [ (heap, [ loc ], [], []) ]

let set_freed heap pfs gamma (loc : vt) =
  let loc_name, new_pfs = resolve_or_create_loc ~pfs ~gamma loc in
  let () = WislSHeap.set_freed heap loc_name in
  ASucc [ (heap, [], new_pfs, []) ]

let rem_freed heap pfs gamma loc =
  with_resolved_loc ~pfs ~gamma loc @@ fun loc_name ->
  match WislSHeap.rem_freed heap loc_name with
  | Error e -> AFail [ e ]
  | Ok () -> ASucc [ (heap, [], [], []) ]

let alloc heap _pfs _gamma (size : int) =
  let loc = WislSHeap.alloc heap size in
  ASucc
    [
      ( heap,
        [ Expr.Lit (Literal.Loc loc); Expr.Lit (Literal.Int Z.zero) ],
        [],
        [] );
    ]

let dispose heap pfs gamma loc =
  with_resolved_loc ~pfs ~gamma loc @@ fun loc_name ->
  match WislSHeap.dispose heap loc_name with
  | Ok () -> ASucc [ (heap, [], [], []) ]
  | Error e -> AFail [ e ]

let execute_action ?unification:_ name heap pfs gamma args =
  let action = WislLActions.ac_from_str name in
  match action with
  | GetCell -> (
      match args with
      | [ loc_expr; offset_expr ] ->
          get_cell heap pfs gamma loc_expr offset_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid GetCell Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | SetCell -> (
      match args with
      | [ loc_expr; offset_expr; value_expr ] ->
          set_cell heap pfs gamma loc_expr offset_expr value_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid SetCell Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | RemCell -> (
      match args with
      | [ loc_expr; offset_expr ] ->
          rem_cell heap pfs gamma loc_expr offset_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid RemCell Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | GetBound -> (
      match args with
      | [ loc_expr ] -> get_bound heap pfs gamma loc_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid GetBound Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | SetBound -> (
      match args with
      | [ loc_expr; Expr.Lit (Int b) ] ->
          set_bound heap pfs gamma loc_expr (Z.to_int b)
      | args ->
          failwith
            (Format.asprintf
               "Invalid SetBound Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | RemBound -> (
      match args with
      | [ loc_expr ] -> rem_bound heap pfs gamma loc_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid RemBound Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | GetFreed -> (
      match args with
      | [ loc_expr ] -> get_freed heap pfs gamma loc_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid GetFreed Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | SetFreed -> (
      match args with
      | [ loc_expr ] -> set_freed heap pfs gamma loc_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid SetFreed Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | RemFreed -> (
      match args with
      | [ loc_expr ] -> rem_freed heap pfs gamma loc_expr
      | args ->
          failwith
            (Format.asprintf
               "Invalid RemFreed Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | Alloc -> (
      match args with
      | [ Expr.Lit (Literal.Int size) ] when Z.geq size Z.one ->
          alloc heap pfs gamma (Z.to_int size)
      | args ->
          failwith
            (Format.asprintf
               "Invalid Alloc Call for WISL, with parameters : [ %a ]"
               (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
               args))
  | Dispose -> (
      match args with
      | [ loc_expr ] -> dispose heap pfs gamma loc_expr
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
let pp_err = WislSHeap.Err.pp

let get_recovery_vals _ (err : WislSHeap.Err.t) =
  match err with
  | MissingResource (_, loc, ofs) ->
      let loc = Expr.loc_from_loc_name loc in
      let ofs = Option.fold ~none:[] ~some:(fun x -> [ x ]) ofs in
      loc :: ofs
  | OutOfBounds (_, loc, ofs) -> [ Expr.loc_from_loc_name loc; ofs ]
  | DoubleFree loc | UseAfterFree loc | MemoryLeak loc ->
      [ Expr.loc_from_loc_name loc ]
  | InvalidLocation e -> [ e ]

let pp_c_fix _ _ = ()
let substitution_in_place ~pfs:_ ~gamma:_ = WislSHeap.substitution_in_place
let fresh_val _ = Expr.LVar (LVar.alloc ())

let clean_up ?(keep = Expr.Set.empty) (mem : t) : Expr.Set.t * Expr.Set.t =
  WislSHeap.clean_up keep mem

let lvars heap = WislSHeap.lvars heap
let alocs heap = WislSHeap.alocs heap
let assertions ?to_keep:_ heap = WislSHeap.assertions heap
let mem_constraints _ = []
let is_overlapping_asrt _ = false
let apply_fix m _ _ _ = m
let get_fixes ?simple_fix:_ _ _ _ _ = []
let get_failing_constraint _ = Formula.True
let add_debugger_variables = WislSHeap.add_debugger_variables
