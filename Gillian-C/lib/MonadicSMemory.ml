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
    | None   -> Delayed.return SHeapTree.empty

  let alloc (mem : t) low high : t * string =
    let loc = ALoc.alloc () in
    let tree = SHeapTree.alloc low high in
    (SMap.add loc tree mem, loc)

  let getcurperm mem loc ofs =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    map_lift_err loc_name (SHeapTree.get_perm_at tree ofs)

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

  let get_single mem loc ofs chunk =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ sval, perm, new_tree =
      map_lift_err loc_name (SHeapTree.get_single tree ofs chunk)
    in
    (SMap.add loc_name new_tree mem, loc_name, sval, perm)

  let set_single mem loc ofs chunk sval perm =
    let open DR.Syntax in
    let* loc_name = resolve_or_create_loc_name loc in
    let* tree = get_or_create_tree mem loc_name in
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.set_single tree ofs chunk sval perm)
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
        if SHeapTree.is_empty new_tree then SMap.remove loc_name mem
        else SMap.add loc_name new_tree mem

  let get_array mem loc ofs size chunk =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ sarr, perm, new_tree =
      map_lift_err loc_name (SHeapTree.get_array tree ofs size chunk)
    in
    (SMap.add loc_name new_tree mem, loc_name, sarr, perm)

  let set_array mem loc ofs size chunk array perm =
    let open DR.Syntax in
    let* loc_name = resolve_or_create_loc_name loc in
    let* tree = get_or_create_tree mem loc_name in
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.set_array tree ofs size chunk array perm)
    in
    SMap.add loc_name new_tree mem

  let rem_array mem loc ofs size chunk =
    let open DR.Syntax in
    let* loc_name = Delayed.resolve_loc loc in
    match Option.bind loc_name (fun l -> SMap.find_opt l mem) with
    | None      -> DR.ok mem
    | Some tree ->
        let loc_name = Option.get loc_name in
        let++ new_tree =
          map_lift_err loc_name (SHeapTree.rem_array tree ofs size chunk)
        in
        if SHeapTree.is_empty new_tree then SMap.remove loc_name mem
        else SMap.add loc_name new_tree mem

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

  let get_simple ~sheap_getter mem loc low high =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ new_tree, perm = map_lift_err loc_name (sheap_getter tree low high) in
    (SMap.add loc_name new_tree mem, loc_name, perm)

  let set_simple ~sheap_setter mem loc low high perm =
    let open DR.Syntax in
    let* loc_name = resolve_or_create_loc_name loc in
    let* tree = get_or_create_tree mem loc_name in
    let++ new_tree = map_lift_err loc_name (sheap_setter tree low high perm) in
    SMap.add loc_name new_tree mem

  let rem_simple ~sheap_remover mem loc low high =
    let open DR.Syntax in
    let* loc_name = Delayed.resolve_loc loc in
    match Option.bind loc_name (fun l -> SMap.find_opt l mem) with
    | None      -> DR.ok mem
    | Some tree ->
        let loc_name = Option.get loc_name in
        let++ new_tree = map_lift_err loc_name (sheap_remover tree low high) in
        if SHeapTree.is_empty new_tree then SMap.remove loc_name mem
        else SMap.add loc_name new_tree mem

  let get_hole = get_simple ~sheap_getter:SHeapTree.get_hole

  let set_hole = set_simple ~sheap_setter:SHeapTree.set_hole

  let rem_hole = rem_simple ~sheap_remover:SHeapTree.rem_hole

  let get_zeros = get_simple ~sheap_getter:SHeapTree.get_zeros

  let set_zeros = set_simple ~sheap_setter:SHeapTree.set_zeros

  let rem_zeros = rem_simple ~sheap_remover:SHeapTree.rem_zeros

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

  let rem_bounds mem loc =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res mem loc_name in
    let++ tree_rem =
      map_lift_err loc_name (DR.of_result (SHeapTree.rem_bounds tree))
    in
    SMap.add loc_name tree_rem mem

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

  (* let merge_locs old_loc new_loc mem =
       let ret_ops =
       let ( let* ) = Option.bind in
       match SMap.find_opt new_loc mem with
       | None ->
         Logging.verbose (fun fmt -> fmt "New location does not exist");
         SMap.find_opt old_loc mem
       | Some new_tree ->
         let* old_tree = SMap.find_opt old_loc mem in
         let def_perm = new_tree.perm in

       in
     match ret_ops with
     | None -> Logging.verbose ?severity:Warning (fun fmt -> fmt -> "Warning: Unable to merge, tree not found at old_loc");
       mem
     | Some tree ->
       SMap.remove old_loc mem |> SMap.add new_loc tree *)

  let substitution subst mem =
    if not (Subst.domain subst None = Expr.Set.empty) then
      let _aloc_subst =
        Subst.fold subst
          (fun l r acc ->
            match l with
            | ALoc aloc -> (aloc, r) :: acc
            | _         -> acc)
          []
      in
      let le_subst = Subst.subst_in_expr subst ~partial:true in
      let sval_subst = SVal.substitution ~le_subst in
      let subst_tree = SHeapTree.substitution ~le_subst ~sval_subst in
      let substituted = SMap.map subst_tree mem in
      (* FIXME: need to merge locations at some point... *)
      (* Subst.fold aloc_subst
         (fun old_loc new_loc acc ->
           Logging.verbose (fun fmt ->
               fmt "SHOULD Merge locs: %s --> %a" old_loc Expr.pp new_loc);
           let _new_loc =
             match new_loc with
             | Lit (Loc loc) | ALoc loc -> loc
             | _                        ->
                 Fmt.failwith "Heap substitution failed for loc : %a" Expr.pp
                   new_loc
           in
           acc) *)
      substituted
    else mem

  let pp ?(genv = GEnv.empty) ft mem =
    let is_fun loc =
      try
        match GEnv.find_def genv loc with
        | FunDef _ -> true
        | _        -> false
      with Not_found -> false
    in
    let iter_exclude f map =
      SMap.iter (fun loc x -> if not (is_fun loc) then f loc x) map
    in
    let open Fmt in
    pf ft "%a" (Dump.iter_bindings iter_exclude nop string SHeapTree.pp) mem
end

type t' = { genv : GEnv.t; mem : Mem.t }

type t = t' ref

type action_ret = Success of (t * vt list) | Failure of err_t

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
  let open DR.Syntax in
  match params with
  | [ loc; ofs ] ->
      let** perm = Mem.getcurperm heap.mem loc ofs in
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
      let** mem, loc_name, sval, perm = Mem.get_single heap.mem loc ofs chunk in
      let loc_e = expr_of_loc_name loc_name in
      let* sval_e = SVal.to_gil_expr sval in
      let perm_string = ValueTranslation.string_of_permission_opt perm in
      DR.ok
        (make_branch ~heap:{ heap with mem }
           ~rets:
             [
               loc_e;
               ofs;
               Expr.Lit (String chunk_string);
               sval_e;
               Expr.Lit (String perm_string);
             ]
           ())
  | _ -> fail_ungracefully "get_single" params

let execute_set_single heap params =
  let open DR.Syntax in
  match params with
  | [
   loc;
   ofs;
   Expr.Lit (String chunk_string);
   sval_e;
   Expr.Lit (String perm_string);
  ] ->
      let perm = ValueTranslation.permission_of_string perm_string in
      let chunk = ValueTranslation.chunk_of_string chunk_string in
      let* sval = SVal.of_gil_expr_exn sval_e in
      let++ mem = Mem.set_single heap.mem loc ofs chunk sval perm in
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

let execute_get_array heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; size; Expr.Lit (String chunk_string) ] ->
      let chunk = ValueTranslation.chunk_of_string chunk_string in
      let** mem, loc_name, array, perm =
        Mem.get_array heap.mem loc ofs size chunk
      in
      let loc_e = expr_of_loc_name loc_name in
      let range = SHeapTree.Range.of_low_chunk_and_size ofs chunk size in
      let* array_e = MonadicSVal.SVArray.to_gil_expr ~chunk ~range array in
      let perm_string = ValueTranslation.string_of_permission_opt perm in
      DR.ok
        (make_branch ~heap:{ heap with mem }
           ~rets:
             [
               loc_e;
               ofs;
               size;
               Expr.Lit (String chunk_string);
               array_e;
               Expr.Lit (String perm_string);
             ]
           ())
  | _ -> fail_ungracefully "get_array" params

let execute_set_array heap params =
  let open DR.Syntax in
  match params with
  | [
   loc;
   ofs;
   size;
   Expr.Lit (String chunk_string);
   arr_e;
   Expr.Lit (String perm_string);
  ] ->
      let perm = ValueTranslation.permission_of_string perm_string in
      let chunk = ValueTranslation.chunk_of_string chunk_string in
      let arr = MonadicSVal.SVArray.of_gil_expr_exn arr_e in
      let++ mem = Mem.set_array heap.mem loc ofs size chunk arr perm in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully "set_single" params

let execute_rem_array heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; size; Expr.Lit (String chunk_string) ] ->
      let chunk = ValueTranslation.chunk_of_string chunk_string in
      let++ mem = Mem.rem_array heap.mem loc ofs size chunk in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully "rem_single" params

let execute_get_simple ~mem_getter ~name heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high ] ->
      let** mem, loc_name, perm = mem_getter heap.mem loc low high in
      let loc_e = expr_of_loc_name loc_name in
      let perm_e =
        Expr.string (ValueTranslation.string_of_permission_opt perm)
      in
      DR.ok
        (make_branch ~heap:{ heap with mem }
           ~rets:[ loc_e; low; high; perm_e ]
           ())
  | _                  -> fail_ungracefully name params

let execute_set_simple ~mem_setter ~name heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high; Expr.Lit (String perm_string) ] ->
      let perm = ValueTranslation.permission_of_string perm_string in
      let++ mem = mem_setter heap.mem loc low high perm in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully name params

let execute_rem_simple ~mem_remover ~name heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high ] ->
      let++ mem = mem_remover heap.mem loc low high in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _                  -> fail_ungracefully name params

let execute_get_hole =
  execute_get_simple ~mem_getter:Mem.get_hole ~name:"get_hole"

let execute_set_hole =
  execute_set_simple ~mem_setter:Mem.set_hole ~name:"set_hole"

let execute_rem_hole =
  execute_rem_simple ~mem_remover:Mem.rem_hole ~name:"rem_hole"

let execute_get_zeros =
  execute_get_simple ~mem_getter:Mem.get_zeros ~name:"get_zeros"

let execute_set_zeros =
  execute_set_simple ~mem_setter:Mem.set_zeros ~name:"set_zeros"

let execute_rem_zeros =
  execute_rem_simple ~mem_remover:Mem.rem_zeros ~name:"rem_zeros"

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
        | Expr.EList [ low; high ]  -> Some (low, high)
        | Lit (LList [ low; high ]) -> Some (Lit low, Lit high)
        | Lit Null                  -> None
        | _                         -> fail_ungracefully "set_bounds" params
      in
      let++ mem = Mem.set_bounds heap.mem loc bounds in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _                 -> fail_ungracefully "set_bounds" params

let execute_rem_bounds heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let++ mem = Mem.rem_bounds heap.mem loc in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _       -> fail_ungracefully "rem_bounds" params

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
  match params with
  | [ Expr.Lit (String symbol); lvar_loc ] ->
      let* loc_name = resolve_or_create_loc_name lvar_loc in
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
  Format.fprintf fmt "GEnv : @[%a@]@\nMem  : @[%a@]" GEnv.pp !h.genv
    (Mem.pp ~genv:!h.genv) !h.mem

let pp_by_need (_ : SS.t) fmt h = pp fmt h

let get_print_info _ _ = (SS.empty, SS.empty)

(* let str_noheap _ = "NO HEAP PRINTED" *)

let lift_res res =
  match res with
  | Ok a    -> Success a
  | Error e -> Failure e

let pp_branch fmt branch =
  let _, values = branch in
  Fmt.pf fmt "Returns: %a@.(Ignoring heap)" (Fmt.Dump.list Expr.pp) values

let lift_dr_and_log res =
  let pp_res = Fmt.Dump.result ~ok:pp_branch ~error:pp_err in
  Delayed.map res (fun res ->
      Logging.tmi (fun fmt -> fmt "Resulting in: %a" pp_res res);
      lift_res res)

(* Actual action execution *)

let execute_action ~action_name heap params =
  Logging.verbose (fun fmt ->
      fmt "Executing action %s with params %a" action_name pp_params params);
  Logging.tmi (fun fmt -> fmt "Current heap : %a" pp heap);
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
    | AMem GetArray   -> execute_get_array !heap params
    | AMem SetArray   -> execute_set_array !heap params
    | AMem RemArray   -> execute_rem_array !heap params
    | AMem GetBounds  -> execute_get_bounds !heap params
    | AMem SetBounds  -> execute_set_bounds !heap params
    | AMem RemBounds  -> execute_rem_bounds !heap params
    | AMem GetHole    -> execute_get_hole !heap params
    | AMem SetHole    -> execute_set_hole !heap params
    | AMem RemHole    -> execute_rem_hole !heap params
    | AMem GetZeros   -> execute_get_zeros !heap params
    | AMem SetZeros   -> execute_set_zeros !heap params
    | AMem RemZeros   -> execute_rem_zeros !heap params
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
  lift_dr_and_log a_ret

(* LActions static *)

let ga_to_setter = LActions.ga_to_setter_str

let ga_to_getter = LActions.ga_to_getter_str

let ga_to_deleter = LActions.ga_to_deleter_str

(* Serialization and operations *)
let substitution_in_place subst heap =
  let { mem; genv } = !heap in
  let genv = GEnv.substitution subst genv in
  let mem = Mem.substitution subst mem in
  heap := { mem; genv }

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

let get_recovery_vals _ = function
  | InvalidLocation e -> [ e ]
  | MissingLocResource l | SHeapTreeErr { at_location = l; _ } ->
      [ Expr.loc_from_loc_name l ]

let get_failing_constraint _e = failwith "Not ready for bi-abduction yet"

let get_fixes ?simple_fix:_ _heap _pfs _gamma _err =
  failwith "Not ready for bi-abduction yet"

let apply_fix _heap _pfs _gamma _fix = failwith "Not ready for bi-abdcution"
