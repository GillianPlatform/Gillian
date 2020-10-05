module GUtils = Gillian.Utils
module Result = Stdlib.Result
open Gillian.Monadic
module DR = Delayed_result
module DO = Delayed_option
open Delayed.Syntax
open Gillian.Symbolic
open Gillian.Gil_syntax
module Logging = Gillian.Logging
module SS = GUtils.Containers.SS
module SVal = MonadicSVal

(* Some utils first *)

let resolve_or_create_loc_name (lvar_loc : Expr.t) : string Delayed.t =
  let open Delayed.Syntax in
  let* loc_name = Delayed.resolve_loc lvar_loc in
  match loc_name with
  | None   ->
      let new_loc_name = ALoc.alloc () in
      let learned = [ Formula.Eq (ALoc new_loc_name, lvar_loc) ] in
      Delayed.return ~learned new_loc_name
  | Some l -> Delayed.return l

let expr_of_loc_name loc_name =
  if GUtils.Names.is_aloc_name loc_name then Expr.ALoc loc_name
  else Lit (Loc loc_name)

type vt = Values.t

type st = Subst.t

type i_fix_t

type err_t =
  | InvalidLocation    of Expr.t
  | MissingLocResource of string
  | SHeapTreeErr       of { at_location : string; sheaptree_err : SHeapTree.err }

let lift_sheaptree_err loc err =
  SHeapTreeErr { at_location = loc; sheaptree_err = err }

let map_lift_err loc res = DR.map_error res (lift_sheaptree_err loc)

let resolve_loc_result loc =
  Delayed_result.of_do ~none:(InvalidLocation loc) (Delayed.resolve_loc loc)

(* let make_err ~fixes ?(fc = Formula.False) ?(rvs = []) () =
  { failing_constraint = fc; recovery_values = rvs; fixes } *)

module Mem = struct
  open Delayed.Syntax
  module SMap = Map.Make (String)

  type t = SHeapTree.t SMap.t

  let empty = SMap.empty

  let copy x = x

  let get_tree_res mem loc_name =
    DR.of_option ~none:(MissingLocResource loc_name)
      (SMap.find_opt loc_name mem)

  let get_or_create_tree mem loc_name =
    match SMap.find_opt loc_name mem with
    | Some t -> Delayed.return t
    | None   -> SHeapTree.empty ()

  let alloc mem low high =
    let loc = ALoc.alloc () in
    let tree = SHeapTree.alloc low high in
    (SMap.add loc tree mem, loc)

  let getcurperm mem loc ofs =
    let open DO.Syntax in
    let** loc_name = Delayed.resolve_loc loc in
    let** tree = DO.of_option (SMap.find_opt loc_name mem) in
    SHeapTree.get_perm_at ofs tree

  let drop_perm mem loc low high new_perm =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ new_tree =
      map_lift_err loc_name
        (DR.of_result (SHeapTree.drop_perm tree low high new_perm))
    in
    SMap.add loc_name new_tree mem

  let store mem loc chunk ofs value =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.store tree chunk ofs value)
    in
    SMap.add loc_name new_tree mem

  let load mem loc chunk ofs =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ value, new_tree =
      map_lift_err loc_name (SHeapTree.load tree chunk ofs)
    in
    (value, SMap.add loc_name new_tree mem)

  let free mem loc low high =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ new_tree = map_lift_err loc_name (SHeapTree.free tree low high) in
    SMap.add loc_name new_tree mem

  let get_single mem loc ofs chunk =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ sval, new_tree =
      map_lift_err loc_name (SHeapTree.get_single tree ofs chunk)
    in
    (SMap.add loc_name new_tree mem, loc_name, sval)

  let set_single mem loc ofs chunk sval =
    let open DR.Syntax in
    let* loc_name = resolve_or_create_loc_name loc in
    let* tree = get_or_create_tree mem loc_name in
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.set_single tree ofs chunk sval)
    in
    SMap.add loc_name new_tree mem

  let rem_single mem loc ofs chunk =
    let open DR.Syntax in
    let* loc_name = Delayed.resolve_loc loc in
    match Option.bind loc_name (fun l -> SMap.find_opt l mem) with
    | None      -> DR.ok mem
    | Some tree ->
        let loc_name = Option.get loc_name in
        let++ new_tree =
          map_lift_err loc_name (SHeapTree.rem_single tree ofs chunk)
        in
        SMap.add loc_name new_tree mem

  let get_freed mem loc =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    DR.of_result (SHeapTree.get_freed tree) |> map_lift_err loc_name

  let set_freed mem loc =
    let+ loc_name = resolve_or_create_loc_name loc in
    SMap.add loc_name SHeapTree.freed mem

  let rem_freed mem loc =
    let open DR.Syntax in
    let* loc_name = Delayed.resolve_loc loc in
    match Option.bind loc_name (fun l -> SMap.find_opt l mem) with
    | None      -> DR.ok mem
    | Some tree ->
        let loc_name = Option.get loc_name in
        let++ () =
          map_lift_err loc_name (DR.of_result (SHeapTree.get_freed tree))
        in
        SMap.remove loc_name mem

  let get_hole mem loc low high =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ new_tree = map_lift_err loc_name (SHeapTree.get_hole tree low high) in
    (SMap.add loc_name new_tree mem, loc_name)

  let set_hole mem loc low high =
    let open DR.Syntax in
    let* loc_name = resolve_or_create_loc_name loc in
    let* tree = get_or_create_tree mem loc_name in
    let++ new_tree = map_lift_err loc_name (SHeapTree.set_hole tree low high) in
    SMap.add loc_name new_tree mem

  let rem_hole mem loc low high =
    let open DR.Syntax in
    let* loc_name = Delayed.resolve_loc loc in
    match Option.bind loc_name (fun l -> SMap.find_opt l mem) with
    | None      -> DR.ok mem
    | Some tree ->
        let loc_name = Option.get loc_name in
        let++ new_tree =
          map_lift_err loc_name (SHeapTree.rem_hole tree low high)
        in
        SMap.add loc_name new_tree mem

  let get_perm mem loc =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ perm =
      map_lift_err loc_name (DR.of_result (SHeapTree.get_perm_res tree))
    in
    (loc_name, perm)

  let set_perm mem loc perm =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ tree_set =
      map_lift_err loc_name (DR.of_result (SHeapTree.set_perm tree perm))
    in
    SMap.add loc_name tree_set mem

  let get_bounds mem loc =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ bounds =
      map_lift_err loc_name (DR.of_result (SHeapTree.get_bounds tree))
    in
    (loc_name, bounds)

  let set_bounds mem loc bounds =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ tree_set =
      map_lift_err loc_name (DR.of_result (SHeapTree.set_bounds tree bounds))
    in
    SMap.add loc_name tree_set mem

  let lvars mem =
    let open Utils.Containers in
    SMap.fold
      (fun _ tree acc -> SS.union (SHeapTree.lvars tree) acc)
      mem SS.empty

  let assertions ~exclude mem =
    SMap.fold
      (fun loc tree acc ->
        if not (List.mem loc exclude) then SHeapTree.assertions ~loc tree @ acc
        else acc)
      mem []

  let pp ft mem =
    let open Fmt in
    pf ft "%a" (Dump.iter_bindings SMap.iter nop string SHeapTree.pp) mem
end

type t' = { genv : GEnv.t; mem : Mem.t }

type t = t' ref

type action_ret = Success of (t * vt list) | Failure of err_t

let lift_res res =
  match res with
  | Ok a    -> Success a
  | Error e -> Failure e

let lift_dr res = Delayed.map res lift_res

let make_branch ~heap ?(rets = []) () = (ref heap, rets)

(* Init *)

let init () = ref { genv = GEnv.empty; mem = Mem.empty }

let copy h = ref { genv = !h.genv; mem = Mem.copy !h.mem }

(* let subst_spec_vars _ _ = () *)

let rec concretize e =
  let open Expr in
  match e with
  | Lit l   -> l
  | EList l -> LList (List.map concretize l)
  | _       ->
      failwith
        (Format.asprintf "param %a should be concrete but isn't" Expr.pp e)

(* Ungraceful failure *)

let pp_params fmt params =
  let rec aux fmtp = function
    | []     -> ()
    | [ a ]  -> Format.fprintf fmt "%a" Expr.pp a
    | a :: r ->
        Format.fprintf fmt "%a, " Expr.pp a;
        aux fmtp r
  in
  Format.fprintf fmt "[%a]" aux params

let fail_ungracefully act_name params =
  failwith (Format.asprintf "Invalid call to %s : %a" act_name pp_params params)

(* Action execution *)

let execute_alloc heap params =
  match params with
  | [ low; high ] ->
      let mem, loc = Mem.alloc heap.mem low high in
      let result =
        make_branch ~heap:{ heap with mem } ~rets:[ Expr.ALoc loc ] ()
      in
      DR.ok result
  | _             -> fail_ungracefully "alloc" params

let execute_getcurperm heap params =
  match params with
  | [ loc; ofs ] ->
      let* perm = Mem.getcurperm heap.mem loc ofs in
      let perm_string =
        Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
      in
      let branch = make_branch ~heap ~rets:[ perm_string ] () in
      DR.ok branch
  | _            -> fail_ungracefully "getcurperm" params

let execute_drop_perm heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high; Expr.Lit (String perm_string) ] ->
      let perm = ValueTranslation.permission_of_string perm_string in
      let++ mem = Mem.drop_perm heap.mem loc low high perm in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully "drop_perm" params

let execute_store heap params =
  let open DR.Syntax in
  match params with
  | [ Expr.Lit (String chunk_name); loc; ofs; value ] ->
      let* sval = SVal.of_gil_expr_exn value in
      let chunk = ValueTranslation.chunk_of_string chunk_name in
      let++ mem = Mem.store heap.mem loc chunk ofs sval in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully "store" params

let execute_load heap params =
  let open DR.Syntax in
  match params with
  | [ Expr.Lit (String chunk_name); loc; ofs ] ->
      let chunk = ValueTranslation.chunk_of_string chunk_name in
      let** value, mem = Mem.load heap.mem loc chunk ofs in
      let* gil_value = SVal.to_gil_expr value in
      DR.ok (make_branch ~heap:{ heap with mem } ~rets:[ gil_value ] ())
  | _ -> fail_ungracefully "store" params

let execute_free heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high ] ->
      let++ mem = Mem.free heap.mem loc low high in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _                  -> fail_ungracefully "free" params

let execute_move _heap params =
  match params with
  | [ _loc_1; _ofs_1; _loc_2; _ofs_2; _size ] ->
      failwith "Move not implemented yet"
  | _ -> fail_ungracefully "wrong call to execute_move" params

let execute_get_single heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; Expr.Lit (String chunk_string) ] ->
      let chunk = ValueTranslation.chunk_of_string chunk_string in
      let** mem, loc_name, sval = Mem.get_single heap.mem loc ofs chunk in
      let loc_e = expr_of_loc_name loc_name in
      let* sval_e = SVal.to_gil_expr sval in
      DR.ok
        (make_branch ~heap:{ heap with mem }
           ~rets:[ loc_e; ofs; Expr.Lit (String chunk_string); sval_e ]
           ())
  | _ -> fail_ungracefully "get_single" params

let execute_set_single heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; Expr.Lit (String chunk_string); sval_e ] ->
      let chunk = ValueTranslation.chunk_of_string chunk_string in
      let* sval = SVal.of_gil_expr_exn sval_e in
      let++ mem = Mem.set_single heap.mem loc ofs chunk sval in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully "set_single" params

let execute_rem_single heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; Expr.Lit (String chunk_string) ] ->
      let chunk = ValueTranslation.chunk_of_string chunk_string in
      let++ mem = Mem.rem_single heap.mem loc ofs chunk in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully "rem_single" params

let execute_get_hole heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high ] ->
      let** mem, loc_name = Mem.get_hole heap.mem loc low high in
      let loc_e = expr_of_loc_name loc_name in
      DR.ok (make_branch ~heap:{ heap with mem } ~rets:[ loc_e; low; high ] ())
  | _                  -> fail_ungracefully "get_hole" params

let execute_set_hole heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high ] ->
      let++ mem = Mem.set_hole heap.mem loc low high in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _                  -> fail_ungracefully "set_hole" params

let execute_rem_hole heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high ] ->
      let++ mem = Mem.rem_hole heap.mem loc low high in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _                  -> fail_ungracefully "rem_hole" params

let execute_get_freed heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let++ () = Mem.get_freed heap.mem loc in
      make_branch ~heap ~rets:[] ()
  | _       -> fail_ungracefully "get_freed" params

let execute_set_freed heap params =
  match params with
  | [ loc ] ->
      let+ mem = Mem.set_freed heap.mem loc in
      Ok (make_branch ~heap:{ heap with mem } ~rets:[] ())
  | _       -> fail_ungracefully "set_freed" params

let execute_rem_freed heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let++ mem = Mem.rem_freed heap.mem loc in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _       -> fail_ungracefully "rem_freed" params

let execute_get_bounds heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let++ loc_name, bounds = Mem.get_bounds heap.mem loc in
      let bounds_e =
        match bounds with
        | None             -> Expr.Lit Null
        | Some (low, high) -> Expr.EList [ low; high ]
      in
      let loc_e = expr_of_loc_name loc_name in
      make_branch ~heap ~rets:[ loc_e; bounds_e ] ()
  | _       -> fail_ungracefully "get_bounds" params

let execute_set_bounds heap params =
  let open DR.Syntax in
  match params with
  | [ loc; bounds_e ] ->
      let bounds =
        match bounds_e with
        | Expr.EList [ low; high ] -> Some (low, high)
        | Lit Null                 -> None
        | _                        -> fail_ungracefully "set_bounds" params
      in
      let++ mem = Mem.set_bounds heap.mem loc bounds in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _                 -> fail_ungracefully "set_bounds" params

let execute_rem_bounds heap params =
  match params with
  | [ _loc ] -> DR.ok (make_branch ~heap ~rets:[] ())
  | _        -> fail_ungracefully "rem_bounds" params

let execute_get_perm heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let++ loc_name, perm = Mem.get_perm heap.mem loc in
      let loc_e = expr_of_loc_name loc_name in
      let perm_string = ValueTranslation.string_of_permission perm in
      make_branch ~heap ~rets:[ loc_e; Expr.string perm_string ] ()
  | _       -> fail_ungracefully "get_perm" params

let execute_set_perm heap params =
  let open DR.Syntax in
  match params with
  | [ loc; Expr.Lit (String perm_string) ] ->
      let perm = ValueTranslation.permission_of_string perm_string in
      let++ mem = Mem.set_perm heap.mem loc perm in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully "set_perm" params

let execute_rem_perm heap params =
  match params with
  | [ _loc ] -> DR.ok (make_branch ~heap ~rets:[] ())
  | _        -> fail_ungracefully "rem_perm" params

let execute_genvgetsymbol heap params =
  let open Gillian.Gil_syntax.Expr in
  match params with
  | [ Lit (String symbol) ] ->
      DR.ok
      @@ make_branch ~heap
           ~rets:
             [
               Lit (String symbol);
               loc_from_loc_name (GEnv.find_symbol heap.genv symbol);
             ]
           ()
  | _                       -> fail_ungracefully "genv_getsymbol" params

let execute_genvsetsymbol heap params =
  let open DR.Syntax in
  match params with
  | [ Expr.Lit (String symbol); lvar_loc ] ->
      let** loc_name = resolve_loc_result lvar_loc in
      let genv = GEnv.set_symbol heap.genv symbol loc_name in
      DR.ok (make_branch ~heap:{ heap with genv } ~rets:[] ())
  | _ -> fail_ungracefully "genv_getsymbol" params

let execute_genvremsymbol heap params =
  match params with
  | [ _symbolc ] -> DR.ok (make_branch ~heap ())
  | _            -> fail_ungracefully "genv_remsymbol" params

let execute_genvgetdef heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let** loc_name = resolve_loc_result loc in
      let def = GEnv.find_def heap.genv loc_name in
      let v = GEnv.serialize_def def in
      DR.ok
        (make_branch ~heap ~rets:[ Expr.loc_from_loc_name loc_name; Lit v ] ())
  | _       -> fail_ungracefully "genv_getdef" params

let execute_genvsetdef heap params =
  match params with
  | [ lvar_loc; v_def ] ->
      let* loc_name = resolve_or_create_loc_name lvar_loc in
      let concrete_def = concretize v_def in
      let def = GEnv.deserialize_def concrete_def in
      let genv = GEnv.set_def heap.genv loc_name def in
      DR.ok (make_branch ~heap:{ heap with genv } ())
  | _                   -> fail_ungracefully "genv_setdef" params

let execute_genvremdef heap params =
  match params with
  | [ _loc ] -> DR.ok (make_branch ~heap ())
  | _        -> fail_ungracefully "genv_remdef" params

(* Complete fixes  *)

type c_fix_t

(* Pretty printing utils *)

let pp_i_fix _fmt _i_fix = failwith "Not ready for bi-abduction yet"

(* let str_of_i_fix i_f = Format.asprintf "%a" pp_i_fix i_f *)

let pp_c_fix _fmt _c_fix = failwith "Not ready for bi-abduction yet"

(* let str_of_c_fix c_f = Format.asprintf "%a" pp_c_fix c_f *)

let pp_err fmt (e : err_t) =
  match e with
  | InvalidLocation loc ->
      Fmt.pf fmt "'%a' cannot be resolved as a location" Expr.pp loc
  | MissingLocResource l ->
      Fmt.pf fmt "No block associated with location '%s'" l
  | SHeapTreeErr { at_location; sheaptree_err } ->
      Fmt.pf fmt "Tree at location '%s' raised: <%a>" at_location
        SHeapTree.pp_err sheaptree_err

(* let str_of_err e = Format.asprintf "%a" pp_err e *)

let pp fmt h =
  Format.fprintf fmt "GEnv : @[%a@]@\nMem  : @[%a@]" GEnv.pp !h.genv Mem.pp
    !h.mem

(* let str_noheap _ = "NO HEAP PRINTED" *)

(* Actual action execution *)

let execute_action ~action_name heap params =
  Logging.verbose (fun fmt ->
      fmt "Executing action %s with params %a" action_name pp_params params);
  let open LActions in
  let a_ret =
    match ac_from_str action_name with
    | AMem Alloc      -> execute_alloc !heap params
    | AMem GetCurPerm -> execute_getcurperm !heap params
    | AMem DropPerm   -> execute_drop_perm !heap params
    | AMem Store      -> execute_store !heap params
    | AMem Load       -> execute_load !heap params
    | AMem Free       -> execute_free !heap params
    | AMem Move       -> execute_move !heap params
    | AMem GetSingle  -> execute_get_single !heap params
    | AMem SetSingle  -> execute_set_single !heap params
    | AMem RemSingle  -> execute_rem_single !heap params
    | AMem GetBounds  -> execute_get_bounds !heap params
    | AMem SetBounds  -> execute_set_bounds !heap params
    | AMem RemBounds  -> execute_rem_bounds !heap params
    | AMem GetPerm    -> execute_get_perm !heap params
    | AMem SetPerm    -> execute_set_perm !heap params
    | AMem RemPerm    -> execute_rem_perm !heap params
    | AMem GetHole    -> execute_get_hole !heap params
    | AMem SetHole    -> execute_set_hole !heap params
    | AMem RemHole    -> execute_rem_hole !heap params
    | AMem GetFreed   -> execute_get_freed !heap params
    | AMem SetFreed   -> execute_set_freed !heap params
    | AMem RemFreed   -> execute_rem_freed !heap params
    | AGEnv GetSymbol -> execute_genvgetsymbol !heap params
    | AGEnv SetSymbol -> execute_genvsetsymbol !heap params
    | AGEnv RemSymbol -> execute_genvremsymbol !heap params
    | AGEnv GetDef    -> execute_genvgetdef !heap params
    | AGEnv SetDef    -> execute_genvsetdef !heap params
    | AGEnv RemDef    -> execute_genvremdef !heap params
  in
  lift_dr a_ret

(* LActions static *)

let ga_to_setter = LActions.ga_to_setter_str

let ga_to_getter = LActions.ga_to_getter_str

let ga_to_deleter = LActions.ga_to_deleter_str

(* Serialization and operations *)
let substitution_in_place _subst _heap = failwith "substitution not implemented"

(* let { mem; genv } = !heap in
   let nmem = Mem.substitution subst mem in
   let ngenv = GEnv.substitution subst genv in
   heap := { mem = nmem; genv = ngenv } *)

let fresh_val _ = Expr.LVar (LVar.alloc ())

let clean_up _ = ()

let lvars heap = Mem.lvars !heap.mem

let assertions ?to_keep:_ heap =
  let genv_locs, genv_asrts = GEnv.assertions !heap.genv in
  let mem_asrts = Mem.assertions ~exclude:genv_locs !heap.mem in
  genv_asrts @ mem_asrts

(* let genv_locs, genv_asrts = GEnv.assertions !heap.genv in
   let mem_asrts = Mem.assertions ~exclude:genv_locs !heap.mem in
   genv_asrts @ mem_asrts *)

let mem_constraints _heap = []

let is_overlapping_asrt = LActions.is_overlapping_asrt_str

let ga_loc_indexes = LActions.ga_loc_indexes_str

(** Things defined for BiAbduction *)

let get_recovery_vals = function
  | InvalidLocation e -> [ e ]
  | MissingLocResource l | SHeapTreeErr { at_location = l; _ } ->
      [ Expr.loc_from_loc_name l ]

let get_failing_constraint _e = failwith "Not ready for bi-abduction yet"

let get_fixes ?simple_fix:_ _heap _pfs _gamma _err =
  failwith "Not ready for bi-abduction yet"

let apply_fix _heap _pfs _gamma _fix = failwith "Not ready for bi-abdcution"
