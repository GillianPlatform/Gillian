open Gillian.Symbolic
open Gillian.Gil_syntax
open Gillian.Logic
module Recovery_tactic = Gillian.General.Recovery_tactic
module Logging = Gillian.Logging
module SFVL = SFVL
module SS = Gillian.Utils.Containers.SS

type init_data = unit
type vt = Values.t
type st = Subst.t
type err_t = WislSHeap.err [@@deriving yojson, show]
type c_fix_t = unit
type t = WislSHeap.t [@@deriving yojson]

type action_ret =
  ( (t * vt list * Formula.t list * (string * Type.t) list) list,
    err_t list )
  result

let init () = WislSHeap.init ()
let clear _ = WislSHeap.init ()

let resolve_loc pfs gamma loc =
  Gillian.Logic.FOSolver.resolve_loc_name ~pfs ~gamma loc

let overwrite_cell pfs gamma (loc : vt) action =
  let loc_name, new_pfs =
    (* If we can't find the location, we create a new location and we
          add to the path condition that it is equal to the given loc *)
    let resolved_loc_opt = resolve_loc pfs gamma loc in
    match resolved_loc_opt with
    | Some loc_name ->
        if Gillian.Utils.Names.is_aloc_name loc_name then (loc_name, [])
        else (loc_name, [])
    | None ->
        let al = ALoc.alloc () in
        (al, [ Formula.Eq (Expr.ALoc al, loc) ])
  in
  action new_pfs loc_name

let store heap pfs gamma (loc : vt) (offset : vt) (value : vt) =
  let action new_pfs loc_name =
    match WislSHeap.store ~pfs ~gamma heap loc_name offset value with
    | Error e -> Error [ e ]
    | Ok () -> Ok [ (heap, [], new_pfs, []) ]
  in
  overwrite_cell pfs gamma loc action

let load heap pfs gamma (loc : vt) (offset : vt) =
  match FOSolver.resolve_loc_name ~pfs ~gamma loc with
  | None -> Error [ WislSHeap.InvalidLocation ]
  | Some loc -> (
      match WislSHeap.load ~pfs ~gamma heap loc offset with
      | Error err -> Error [ err ]
      | Ok value -> Ok [ (heap, [ value ], [], []) ])

let get_cell heap pfs gamma (loc : vt) (offset : vt) permission =
  match resolve_loc pfs gamma loc with
  | None -> Error [ WislSHeap.InvalidLocation ]
  | Some loc -> (
      match WislSHeap.get_cell ~pfs ~gamma heap loc offset permission with
      | Error err -> Error [ err ]
      | Ok (loc, ofs, q, value) ->
          let loc = Expr.loc_from_loc_name loc in
          Ok [ (heap, [ loc; ofs; q; value ], [], []) ])

let set_cell heap pfs gamma (loc : vt) (offset : vt) (value : vt) permission =
  let action new_pfs loc_name =
    match
      WislSHeap.set_cell ~pfs ~gamma heap loc_name offset value permission
    with
    | Error e -> Error [ e ]
    | Ok fls -> Ok [ (heap, [], fls @ new_pfs, []) ]
  in
  overwrite_cell pfs gamma loc action

let rem_cell heap pfs gamma (loc : vt) (offset : vt) permission =
  match FOSolver.resolve_loc_name ~pfs ~gamma loc with
  | Some loc_name -> (
      match WislSHeap.rem_cell ~pfs ~gamma heap loc_name offset permission with
      | Error e -> Error [ e ]
      | Ok () -> Ok [ (heap, [], [], []) ])
  | None ->
      (* loc does not evaluate to a location, or we can't find it. *)
      Error [ InvalidLocation ]

let get_bound heap pfs gamma loc permission =
  match FOSolver.resolve_loc_name ~pfs ~gamma loc with
  | Some loc_name -> (
      match WislSHeap.get_bound ~pfs ~gamma heap loc_name permission with
      | Error e -> Error [ e ]
      | Ok (b, perm) ->
          let b = Expr.int b in
          let loc = Expr.loc_from_loc_name loc_name in
          Ok [ (heap, [ loc; perm; b ], [], []) ])
  | None -> Error [ InvalidLocation ]

let set_bound heap pfs gamma (loc : vt) (bound : int) permission =
  let loc_name, new_pfs =
    (* If we can't find the location, we create a new location and we
         add to the path condition that it is equal to the given loc *)
    let resolved_loc_opt = resolve_loc pfs gamma loc in
    match resolved_loc_opt with
    | Some loc_name ->
        if Gillian.Utils.Names.is_aloc_name loc_name then (loc_name, [])
        else (loc_name, [])
    | None ->
        let al = ALoc.alloc () in
        (al, [ Formula.Eq (Expr.ALoc al, loc) ])
  in
  match WislSHeap.set_bound heap loc_name bound permission with
  | Error e -> Error [ e ]
  | Ok fls -> Ok [ (heap, [], fls @ new_pfs, []) ]

let rem_bound heap pfs gamma loc permission =
  match FOSolver.resolve_loc_name ~pfs ~gamma loc with
  | Some loc_name -> (
      match WislSHeap.rem_bound ~pfs ~gamma heap loc_name permission with
      | Error e -> Error [ e ]
      | Ok () -> Ok [ (heap, [], [], []) ])
  | None ->
      (* loc does not evaluate to a location, or we can't find it. *)
      Error [ InvalidLocation ]

let get_freed heap pfs gamma loc =
  match FOSolver.resolve_loc_name ~pfs ~gamma loc with
  | Some loc_name -> (
      match WislSHeap.get_freed heap loc_name with
      | Error e -> Error [ e ]
      | Ok () ->
          let loc = Expr.loc_from_loc_name loc_name in
          Ok [ (heap, [ loc ], [], []) ])
  | None -> Error [ InvalidLocation ]

let set_freed heap pfs gamma (loc : vt) =
  let loc_name, new_pfs =
    (* If we can't find the location, we create a new location and we
         add to the path condition that it is equal to the given loc *)
    let resolved_loc_opt = resolve_loc pfs gamma loc in
    match resolved_loc_opt with
    | Some loc_name ->
        if Gillian.Utils.Names.is_aloc_name loc_name then (loc_name, [])
        else (loc_name, [])
    | None ->
        let al = ALoc.alloc () in
        (al, [ Formula.Eq (Expr.ALoc al, loc) ])
  in
  let () = WislSHeap.set_freed heap loc_name in
  Ok [ (heap, [], new_pfs, []) ]

let rem_freed heap pfs gamma loc =
  match FOSolver.resolve_loc_name ~pfs ~gamma loc with
  | Some loc_name -> (
      match WislSHeap.rem_freed heap loc_name with
      | Error e -> Error [ e ]
      | Ok () -> Ok [ (heap, [], [], []) ])
  | None ->
      (* loc does not evaluate to a location, or we can't find it. *)
      Error [ InvalidLocation ]

let alloc heap _pfs _gamma (size : int) =
  let loc = WislSHeap.alloc heap size in
  Ok
    [
      ( heap,
        [ Expr.Lit (Literal.Loc loc); Expr.Lit (Literal.Int Z.zero) ],
        [],
        [] );
    ]

let dispose heap pfs gamma loc_expr =
  match resolve_loc pfs gamma loc_expr with
  | Some loc_name -> (
      match WislSHeap.dispose heap ~pfs ~gamma loc_name with
      | Ok () -> Ok [ (heap, [], [], []) ]
      | Error e -> Error [ e ])
  | None -> Error [ InvalidLocation ]

let execute_action ?unification:_ name heap pfs gamma args =
  let action = WislLActions.ac_from_str name in
  let ret =
    match action with
    | Store -> (
        match args with
        | [ loc_expr; offset_expr; value_expr ] ->
            store heap pfs gamma loc_expr offset_expr value_expr
        | args ->
            failwith
              (Format.asprintf
                 "Invalid Store Call for WISL, with parameters : [ %a ]"
                 (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
                 args))
    | Load -> (
        match args with
        | [ loc_expr; offset_expr ] -> load heap pfs gamma loc_expr offset_expr
        | args ->
            failwith
              (Format.asprintf
                 "Invalid Load Call for WISL, with parameters : [ %a ]"
                 (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
                 args))
    | GetCell -> (
        match args with
        | [ loc_expr; offset_expr; permission ] ->
            get_cell heap pfs gamma loc_expr offset_expr permission
        | args ->
            failwith
              (Format.asprintf
                 "Invalid GetCell Call for WISL, with parameters : [ %a ]"
                 (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
                 args))
    | SetCell -> (
        match args with
        | [ loc_expr; offset_expr; permission; value_expr ] ->
            set_cell heap pfs gamma loc_expr offset_expr value_expr permission
        | args ->
            failwith
              (Format.asprintf
                 "Invalid SetCell Call for WISL, with parameters : [ %a ]"
                 (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
                 args))
    | RemCell -> (
        match args with
        | [ loc_expr; offset_expr; permission ] ->
            rem_cell heap pfs gamma loc_expr offset_expr permission
        | args ->
            failwith
              (Format.asprintf
                 "Invalid RemCell Call for WISL, with parameters : [ %a ]"
                 (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
                 args))
    | GetBound -> (
        match args with
        | [ loc_expr; permission ] ->
            get_bound heap pfs gamma loc_expr permission
        | args ->
            failwith
              (Format.asprintf
                 "Invalid GetBound Call for WISL, with parameters : [ %a ]"
                 (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
                 args))
    | SetBound -> (
        match args with
        | [ loc_expr; permission; Expr.Lit (Int b) ] ->
            set_bound heap pfs gamma loc_expr (Z.to_int b) permission
        | args ->
            failwith
              (Format.asprintf
                 "Invalid SetBound Call for WISL, with parameters : [ %a ]"
                 (WPrettyUtils.pp_list ~sep:(format_of_string "; ") Values.pp)
                 args))
    | RemBound -> (
        match args with
        | [ loc_expr; permission ] ->
            rem_bound heap pfs gamma loc_expr permission
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
  in
  Logging.verbose (fun m ->
      m "Action %s resulted in %a"
        (WislLActions.str_ac action)
        Fmt.(
          result
            ~ok:
              (Dump.list (fun ft (h, vs, fs, tys) ->
                   pf ft "(%a, %a, %a, %a)" WislSHeap.pp h (Dump.list Expr.pp)
                     vs (Dump.list Formula.pp) fs
                     (Dump.list @@ Dump.pair string Type.pp)
                     tys))
            ~error:(Dump.list WislSHeap.pp_err))
        ret);
  ret

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
  Fmt.string fmt
    (match t with
    | WislSHeap.MissingResource _ -> "Missing Resource"
    | DoubleFree _ -> "Double Free"
    | UseAfterFree _ -> "Use After Free"
    | MemoryLeak -> "Memory Leak"
    | OutOfBounds _ -> "Out Of Bounds"
    | InvalidLocation -> "Invalid Location")

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
let get_fixes _ _ _ _ = []

let get_recovery_tactic _ e =
  match e with
  | WislSHeap.MissingResource (_, loc, ofs) ->
      let loc = Expr.loc_from_loc_name loc in
      let ofs = Option.to_list ofs in
      Recovery_tactic.try_unfold (loc :: ofs)
  | _ -> Recovery_tactic.none

let get_failing_constraint _ = Formula.True
let add_debugger_variables = WislSHeap.add_debugger_variables
