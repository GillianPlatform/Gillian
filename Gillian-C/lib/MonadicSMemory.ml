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
      map_lift_err loc_name (SHeapTree.drop_perm tree low high new_perm)
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

  let get mem loc ofs chunk =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ sval, new_tree =
      map_lift_err loc_name (SHeapTree.get tree ofs chunk)
    in
    (SMap.add loc_name new_tree mem, loc_name, sval)

  let set mem loc ofs chunk sval =
    let open DR.Syntax in
    let* loc_name = resolve_or_create_loc_name loc in
    let** tree = get_tree_res mem loc_name in
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.set tree ofs chunk sval)
    in
    SMap.add loc_name new_tree mem

  let lvars mem =
    let open Utils.Containers in
    SMap.fold
      (fun _ tree acc -> SS.union (SHeapTree.lvars tree) acc)
      mem SS.empty

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

let execute_mem_get heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; Expr.Lit (String chunk_string) ] ->
      let chunk = ValueTranslation.chunk_of_string chunk_string in
      let** mem, loc_name, sval = Mem.get heap.mem loc ofs chunk in
      let loc_e = expr_of_loc_name loc_name in
      let* sval_e = SVal.to_gil_expr sval in
      DR.ok
        (make_branch ~heap:{ heap with mem }
           ~rets:[ loc_e; ofs; Expr.Lit (String chunk_string); sval_e ]
           ())
  | _ -> fail_ungracefully "mem_get" params

let execute_mem_set heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; Expr.Lit (String chunk_string); sval_e ] ->
      let chunk = ValueTranslation.chunk_of_string chunk_string in
      let* sval = SVal.of_gil_expr_exn sval_e in
      let++ mem = Mem.set heap.mem loc ofs chunk sval in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully "mem_set" params

let execute_mem_rem _heap params =
  match params with
  | [ _loc; _ofs; Expr.Lit (String _chunk_string) ] ->
      failwith "Mem_rem isn't implemented yet"
  | _ -> fail_ungracefully "mem_rem" params

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
    | AMem MGet       -> execute_mem_get !heap params
    | AMem MSet       -> execute_mem_set !heap params
    | AMem MRem       -> execute_mem_rem !heap params
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

let assertions ?to_keep:_ _heap = failwith "assertions not implemented"

(* let genv_locs, genv_asrts = GEnv.assertions !heap.genv in
   let mem_asrts = Mem.assertions ~exclude:genv_locs !heap.mem in
   genv_asrts @ mem_asrts *)

let mem_constraints _heap = []

let is_overlapping_asrt = LActions.is_overlapping_asrt_str

let ga_loc_indexes = LActions.ga_loc_indexes_str

(** Things defined for BiAbduction *)

let get_recovery_vals _e = failwith "Not ready for bi-abduction yet"

let get_failing_constraint _e = failwith "Not ready for bi-abduction yet"

let get_fixes ?simple_fix:_ _heap _pfs _gamma _err =
  failwith "Not ready for bi-abduction yet"

let apply_fix _heap _pfs _gamma _fix = failwith "Not ready for bi-abdcution"
