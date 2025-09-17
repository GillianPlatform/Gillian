module GUtils = Gillian.Utils
module Result = Stdlib.Result
open Gillian.Monadic
open LActions
module DR = Delayed_result
module DO = Delayed_option
open Delayed.Syntax
open Gillian.Symbolic
open Gillian.Gil_syntax
module Logging = Gillian.Logging
module PFS = Gillian.Symbolic.Pure_context
module SS = GUtils.Containers.SS
module SVal = MonadicSVal
module Debugger = Gillian.Debugger
module Recovery_tactic = Gillian.General.Recovery_tactic

(* Some utils first *)

type init_data = Global_env.t

let resolve_or_create_loc_name (lvar_loc : Expr.t) : string Delayed.t =
  let open Delayed.Syntax in
  let* loc_name = Delayed.resolve_loc lvar_loc in
  match loc_name with
  | None ->
      let new_loc_name = ALoc.alloc () in
      let learned = [ Expr.BinOp (ALoc new_loc_name, Equal, lvar_loc) ] in
      Logging.verbose (fun fmt ->
          fmt "Couldn't resolve loc %a, created %s" Expr.pp lvar_loc
            new_loc_name);
      Delayed.return ~learned new_loc_name
  | Some l ->
      Logging.verbose (fun fmt -> fmt "Resolved %a as %s" Expr.pp lvar_loc l);
      Delayed.return l

(* let ga = LActions.ga *)

type err_t =
  | InvalidLocation of Expr.t
  | MissingLocResource of {
      is_store : bool;
      action : LActions.ga;
      loc_name : string;
      ofs_opt : Expr.t option;
      chunk_opt : Chunk.t option;
    }
  | SHeapTreeErr of {
      at_locations : string list;
      sheaptree_err : SHeapTree.err;
    }
[@@deriving show, yojson]

let lift_sheaptree_err loc err =
  SHeapTreeErr { at_locations = [ loc ]; sheaptree_err = err }

let resolve_loc_result loc =
  Delayed_result.of_do ~none:(InvalidLocation loc) (Delayed.resolve_loc loc)

(* let make_err ~fixes ?(fc = Formula.False) ?(rvs = []) () =
   { failing_constraint = fc; recovery_values = rvs; fixes } *)

module Mem = struct
  open Delayed.Syntax

  module SMap = GUtils.Prelude.Map.Make (struct
    include String

    let of_yojson = function
      | `String s -> Ok s
      | _ -> Error "string_of_yojson: expected string"

    let to_yojson s = `String s
  end)

  type t = SHeapTree.t SMap.t

  let of_yojson json = SMap.of_yojson SHeapTree.of_yojson json
  let to_yojson map = SMap.to_yojson SHeapTree.to_yojson map
  let map_lift_err loc res = DR.map_error res (lift_sheaptree_err loc)
  let empty = SMap.empty

  let get_tree_res ?(is_store = false) map loc_name ofs_opt chunk_opt =
    DR.of_option
      ~none:
        (MissingLocResource
           { is_store; action = Single; loc_name; ofs_opt; chunk_opt })
      (SMap.find_opt loc_name map)

  let get_or_create_tree map loc_name =
    match SMap.find_opt loc_name map with
    | Some t -> Delayed.return t
    | None -> Delayed.return SHeapTree.empty

  let update_loc loc tree map =
    if SHeapTree.is_empty tree then SMap.remove loc map
    else SMap.add loc tree map

  (***** Implementation of local actions *****)

  let alloc (map : t) low high : t * string =
    let loc = ALoc.alloc () in
    let tree = SHeapTree.alloc low high in
    (SMap.add loc tree map, loc)

  let weak_valid_pointer map loc ofs =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    (* The corresponding missing error should be a missing bound *)
    let** tree = get_tree_res map loc_name (Some ofs) None in
    map_lift_err loc_name (SHeapTree.weak_valid_pointer tree ofs)

  let getcurperm map loc ofs =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    (* The corresponding missing error should be a missing bound.
       Though getcurperm is only used for pointer validity checking,
       we should move that logic to something more fixable. *)
    let** tree = get_tree_res map loc_name (Some ofs) None in
    map_lift_err loc_name (SHeapTree.get_perm_at tree ofs)

  let drop_perm map loc low high new_perm =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name None None in
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.drop_perm tree low high new_perm)
    in
    SMap.add loc_name new_tree map

  let store map loc chunk ofs value =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree =
      get_tree_res ~is_store:true map loc_name (Some ofs) (Some chunk)
    in
    Logging.verbose (fun m -> m "Got inside store -  %a" Expr.pp ofs);
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.store tree chunk ofs value)
    in
    SMap.add loc_name new_tree map

  let load map loc chunk ofs =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name (Some ofs) (Some chunk) in
    let++ value, new_tree =
      map_lift_err loc_name (SHeapTree.load tree chunk ofs)
    in
    (value, SMap.add loc_name new_tree map)

  let free map loc low high =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name None None in
    let++ new_tree = map_lift_err loc_name (SHeapTree.free tree low high) in
    SMap.add loc_name new_tree map

  let cons_single map loc ofs chunk =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name (Some ofs) (Some chunk) in
    let++ sval, perm, new_tree =
      map_lift_err loc_name (SHeapTree.cons_single tree ofs chunk)
    in
    (update_loc loc_name new_tree map, sval, perm)

  let prod_single map loc ofs chunk sval perm =
    let open DR.Syntax in
    let* loc_name = resolve_or_create_loc_name loc in
    let* tree = get_or_create_tree map loc_name in
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.prod_single tree ofs chunk sval perm)
    in
    SMap.add loc_name new_tree map

  let cons_array map loc ofs size chunk =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let open Expr.Infix in
    if%sat size <= Expr.int 0 then
      DR.ok (map, MonadicSVal.SVArray.empty, Some Perm.Freeable)
    else
      let** tree = get_tree_res map loc_name (Some ofs) (Some chunk) in
      let++ sarr, perm, new_tree =
        map_lift_err loc_name (SHeapTree.cons_array tree ofs size chunk)
      in
      (update_loc loc_name new_tree map, sarr, perm)

  let prod_array map loc ofs size chunk array perm =
    let open DR.Syntax in
    let open Expr.Infix in
    if%sat size <= Expr.int 0 then DR.ok map
    else
      let* loc_name = resolve_or_create_loc_name loc in
      let* tree = get_or_create_tree map loc_name in
      let++ new_tree =
        map_lift_err loc_name
          (SHeapTree.prod_array tree ofs size chunk array perm)
      in
      Logging.tmi (fun m -> m "created tree: %a" SHeapTree.pp new_tree);
      SMap.add loc_name new_tree map

  let cons_freed map loc =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name None None in
    (* TODO: remove the thing in the map *)
    if SHeapTree.is_freed tree then DR.ok (SMap.remove loc_name map)
    else DR.error (lift_sheaptree_err loc_name SHeapTree.MemoryNotFreed)

  let prod_freed map loc =
    let+ loc_name = resolve_or_create_loc_name loc in
    SMap.add loc_name SHeapTree.freed map

  let cons_simple ~sheap_consumer map loc low high =
    let open DR.Syntax in
    let open Expr.Infix in
    let** loc_name = resolve_loc_result loc in
    if%sat high <= low then DR.ok (map, Some Perm.Freeable)
    else
      let** tree = get_tree_res map loc_name None None in
      let++ new_tree, perm =
        map_lift_err loc_name (sheap_consumer tree low high)
      in
      (update_loc loc_name new_tree map, perm)

  let prod_simple ~sheap_producer map loc low high perm =
    let open DR.Syntax in
    let open Expr.Infix in
    if%sat high <= low then DR.ok map
    else
      let* loc_name = resolve_or_create_loc_name loc in
      let* tree = get_or_create_tree map loc_name in
      let++ new_tree =
        map_lift_err loc_name (sheap_producer tree low high perm)
      in
      SMap.add loc_name new_tree map

  let cons_hole = cons_simple ~sheap_consumer:SHeapTree.cons_hole
  let prod_hole = prod_simple ~sheap_producer:SHeapTree.prod_hole

  let allocate_function map loc =
    let tree = SHeapTree.allocated_function in
    SMap.add loc tree map

  let cons_zeros = cons_simple ~sheap_consumer:SHeapTree.cons_zeros
  let prod_zeros = prod_simple ~sheap_producer:SHeapTree.prod_zeros

  let cons_bounds map loc =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name None None in
    let++ bounds, new_tree =
      SHeapTree.cons_bounds tree |> DR.of_result |> map_lift_err loc_name
    in
    (update_loc loc_name new_tree map, bounds)

  let prod_bounds map loc bounds =
    let open DR.Syntax in
    let* loc_name = resolve_or_create_loc_name loc in
    let* tree = get_or_create_tree map loc_name in
    let++ tree_set =
      SHeapTree.prod_bounds tree bounds |> DR.of_result |> map_lift_err loc_name
    in
    SMap.add loc_name tree_set map

  let move map dst_loc dst_ofs src_loc src_ofs sz =
    let open DR.Syntax in
    let open Expr.Infix in
    if%sat sz == Expr.int 0 then DR.ok map
    else
      let** dst_loc_name = resolve_loc_result dst_loc in
      let** src_loc_name = resolve_loc_result src_loc in
      let** dst_tree = get_tree_res map dst_loc_name (Some dst_ofs) None in
      let** src_tree = get_tree_res map src_loc_name (Some src_ofs) None in
      let++ new_dst_tree =
        DR.map_error (SHeapTree.move dst_tree dst_ofs src_tree src_ofs sz)
          (fun err ->
            SHeapTreeErr
              {
                at_locations = [ dst_loc_name; src_loc_name ];
                sheaptree_err = err;
              })
      in
      SMap.add dst_loc_name new_dst_tree map

  let lvars map =
    let open Utils.Containers in
    SMap.fold
      (fun _ tree acc -> SS.union (SHeapTree.lvars tree) acc)
      map SS.empty

  let alocs map =
    let open Utils.Containers in
    SMap.fold
      (fun _ tree acc -> SS.union (SHeapTree.alocs tree) acc)
      map SS.empty

  let assertions ~exclude map =
    SMap.fold
      (fun loc tree acc ->
        if exclude loc then acc else SHeapTree.assertions ~loc tree @ acc)
      map []

  let pp_full ft mem =
    let open Fmt in
    (Dump.iter_bindings SMap.iter nop string SHeapTree.pp_full) ft mem

  let substitution subst map : (t, SHeapTree.err) DR.t =
    let open DR.Syntax in
    if Subst.domain subst None = Expr.Set.empty then DR.ok map
    else
      let aloc_subst =
        Subst.fold subst
          (fun l r acc ->
            match l with
            | ALoc aloc -> (aloc, r) :: acc
            | _ -> acc)
          []
      in
      let le_subst = Subst.subst_in_expr subst ~partial:true in
      let sval_subst = SVal.substitution ~le_subst in
      let svarr_subst = SVal.SVArray.subst ~le_subst in
      let subst_tree =
        SHeapTree.substitution ~le_subst ~sval_subst ~svarr_subst
      in
      let substituted = SMap.map subst_tree map in
      List.fold_left
        (fun acc (old_loc, new_loc) ->
          let** acc = acc in
          Logging.verbose (fun fmt ->
              fmt "SHOULD Merge locs: %s --> %a" old_loc Expr.pp new_loc);
          Logging.tmi (fun fmt -> fmt "IN MEMORY: %a" pp_full acc);
          let new_loc =
            match new_loc with
            | Lit (Loc loc) | ALoc loc -> loc
            | _ ->
                Fmt.failwith "Heap substitution failed for loc : %a" Expr.pp
                  new_loc
          in
          match SMap.find_opt new_loc acc with
          | Some new_tree -> (
              try
                let old_tree = SMap.find old_loc acc in
                let without_old = SMap.remove old_loc acc in
                Logging.verbose (fun fmt -> fmt "Merging now.");
                let++ merged = SHeapTree.merge ~new_tree ~old_tree in
                Logging.verbose (fun fmt -> fmt "Done merging.");
                SMap.add new_loc merged without_old
              with Not_found -> DR.ok acc)
          | None -> (
              try
                let tree = SMap.find old_loc acc in
                DR.ok (SMap.add new_loc tree (SMap.remove old_loc acc))
              with Not_found -> DR.ok acc))
        (DR.ok substituted) aloc_subst

  let pp_normal ~exclude ft mem =
    let is_first = ref true in
    SMap.iter
      (fun loc tree ->
        if not (exclude loc) then (
          if !is_first then is_first := false else Fmt.pf ft "@\n";
          Fmt.pf ft "%s -> @[<v 0>%a@]" loc SHeapTree.pp tree))
      mem

  let pp ~exclude fmt map =
    if !Config.pp_full_tree then pp_full fmt map else pp_normal ~exclude fmt map
end

type t = { genv : Global_env.t; mem : Mem.t }

let to_yojson t =
  let { genv = _; mem } = t in
  Mem.to_yojson mem

let of_yojson m =
  Result.map (fun mem -> { genv = Global_env.empty; mem }) (Mem.of_yojson m)

let make_branch ~heap ?(rets = []) () = (heap, rets)

(* Init *)

let just_functions genv =
  if !Config.allocated_functions then
    String_map.fold
      (fun loc def acc ->
        match def with
        | Global_env.FunDef _ -> Mem.allocate_function acc loc
        | GlobVar _ -> acc)
      genv Mem.empty
  else Mem.empty

let init genv = { genv; mem = just_functions genv }

let sure_is_nonempty state =
  let is_genv loc = String_map.find_opt loc state.genv |> Option.is_some in
  let is_empty =
    Mem.SMap.for_all
      (fun loc tree -> is_genv loc || SHeapTree.is_empty_or_freed tree)
      state.mem
  in
  not is_empty

let get_init_data { genv; _ } = genv
let clear { genv; _ } = { genv; mem = just_functions genv }
let copy h = h

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
  | _ -> fail_ungracefully "alloc" params

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
  | _ -> fail_ungracefully "getcurperm" params

let execute_weak_valid_pointer heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs ] ->
      let** bool = Mem.weak_valid_pointer heap.mem loc ofs in
      let res = Expr.bool bool in
      let branch = make_branch ~heap ~rets:[ res ] () in
      DR.ok branch
  | _ -> fail_ungracefully "weakvalidpointer" params

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
  | _ -> fail_ungracefully "free" params

let execute_move heap params =
  let open DR.Syntax in
  match params with
  | [ dst_loc; dst_ofs; src_loc; src_ofs; size ] ->
      let++ mem = Mem.move heap.mem dst_loc dst_ofs src_loc src_ofs size in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully "wrong call to execute_move" params

let execute_cons_single heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; Expr.Lit (String chunk_string) ] ->
      let chunk = ValueTranslation.chunk_of_string chunk_string in
      let** mem, sval, perm = Mem.cons_single heap.mem loc ofs chunk in
      let* sval_e = SVal.to_gil_expr sval in
      let perm_string = ValueTranslation.string_of_permission_opt perm in
      DR.ok
        (make_branch ~heap:{ heap with mem }
           ~rets:[ sval_e; Expr.Lit (String perm_string) ]
           ())
  | _ -> fail_ungracefully "cons_single" params

let execute_prod_single heap params =
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
      let* sval = SVal.of_gil_expr_vanish sval_e in
      let++ mem = Mem.prod_single heap.mem loc ofs chunk sval perm in
      { heap with mem }
  | _ -> fail_ungracefully "set_single" params

let execute_cons_array heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; size; Expr.Lit (String chunk_string) ] ->
      let chunk = ValueTranslation.chunk_of_string chunk_string in
      let** mem, array, perm = Mem.cons_array heap.mem loc ofs size chunk in
      let range = SHeapTree.Range.of_low_chunk_and_size ofs chunk size in
      let* array_e = MonadicSVal.SVArray.to_gil_expr ~chunk ~range array in
      let perm_string = ValueTranslation.string_of_permission_opt perm in
      DR.ok
        (make_branch ~heap:{ heap with mem }
           ~rets:[ array_e; Expr.Lit (String perm_string) ]
           ())
  | _ -> fail_ungracefully "get_array" params

let execute_prod_array heap params =
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
      let++ mem = Mem.prod_array heap.mem loc ofs size chunk arr perm in
      { heap with mem }
  | _ -> fail_ungracefully "set_single" params

let execute_cons_simple ~mem_consumer ~name heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high ] ->
      let** mem, perm = mem_consumer heap.mem loc low high in
      let perm_e =
        Expr.string (ValueTranslation.string_of_permission_opt perm)
      in
      DR.ok (make_branch ~heap:{ heap with mem } ~rets:[ perm_e ] ())
  | _ -> fail_ungracefully name params

let execute_prod_simple ~mem_producer ~name heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high; Expr.Lit (String perm_string) ] ->
      let perm = ValueTranslation.permission_of_string perm_string in
      let++ mem = mem_producer heap.mem loc low high perm in
      { heap with mem }
  | _ -> fail_ungracefully name params

let execute_cons_hole =
  execute_cons_simple ~mem_consumer:Mem.cons_hole ~name:"get_hole"

let execute_prod_hole =
  execute_prod_simple ~mem_producer:Mem.prod_hole ~name:"set_hole"

let execute_cons_zeros =
  execute_cons_simple ~mem_consumer:Mem.cons_zeros ~name:"get_zeros"

let execute_prod_zeros =
  execute_prod_simple ~mem_producer:Mem.prod_zeros ~name:"set_zeros"

let execute_cons_freed heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let++ mem = Mem.cons_freed heap.mem loc in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully "cons_freed" params

let execute_prod_freed heap params =
  match params with
  | [ loc ] ->
      let+ mem = Mem.prod_freed heap.mem loc in
      { heap with mem }
  | _ -> fail_ungracefully "prod_freed" params

let execute_cons_bounds heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let++ mem, bounds = Mem.cons_bounds heap.mem loc in
      let bounds_e =
        match bounds with
        | None -> Expr.Lit Null
        | Some (low, high) -> Expr.EList [ low; high ]
      in
      make_branch ~heap:{ heap with mem } ~rets:[ bounds_e ] ()
  | _ -> fail_ungracefully "get_bounds" params

let execute_prod_bounds heap params =
  let open DR.Syntax in
  match params with
  | [ loc; bounds_e ] ->
      let bounds =
        match bounds_e with
        | Expr.EList [ low; high ] -> Some (low, high)
        | Lit (LList [ low; high ]) -> Some (Lit low, Lit high)
        | Lit Null -> None
        | _ -> fail_ungracefully "set_bounds" params
      in
      let++ mem = Mem.prod_bounds heap.mem loc bounds in
      { heap with mem }
  | _ -> fail_ungracefully "set_bounds" params

let execute_genvgetdef heap params =
  match params with
  | [ (Expr.Lit (Loc loc) | Expr.ALoc loc | Expr.LVar loc) ] -> (
      match Global_env.find_def_opt heap.genv loc with
      | Some def ->
          let v = Global_env.serialize_def def in
          DR.ok (make_branch ~heap ~rets:[ Expr.Lit (Loc loc); Expr.Lit v ] ())
      | None ->
          (* If we can't find a function, in UX mode we give up, while in OX mode we
             signal. *)
          if !Gillian.Utils.Config.under_approximation then Delayed.vanish ()
          else
            Fmt.failwith "execute_genvgetdef: couldn't find %s\nGENV:\n%a" loc
              Global_env.pp heap.genv)
  | _ -> fail_ungracefully "genv_getdef" params

(* Complete fixes  *)

(* Pretty printing utils *)

let pp_err fmt (e : err_t) =
  match e with
  | InvalidLocation loc ->
      Fmt.pf fmt "[InvalidLocation] '%a' cannot be resolved as a location"
        Expr.pp loc
  | MissingLocResource { is_store; action; loc_name; ofs_opt; chunk_opt } ->
      Fmt.pf fmt
        "[MissingLocResource] No block associated with location '%s'. \
         Associated data: \n\
        \ * is_store: '%B'\n\
        \ * location: '%s'\n\
        \ * core_pred: %s\n\
        \ * value: %a \n\
        \ * chunk: %a \n"
        loc_name is_store loc_name (LActions.str_ga action)
        (Fmt.Dump.option Expr.pp) ofs_opt (Fmt.Dump.option Chunk.pp) chunk_opt
  | SHeapTreeErr { at_locations; sheaptree_err } ->
      Fmt.pf fmt "[SHeapTreeErr] Tree at location%a raised: <%a>"
        (fun fmt l ->
          match l with
          | [ s ] -> Fmt.pf fmt " '%s'" s
          | l -> Fmt.pf fmt "s %a" (Fmt.Dump.list Fmt.string) l)
        at_locations SHeapTree.pp_err sheaptree_err

(* let str_of_err e = Format.asprintf "%a" pp_err e *)

let pp fmt h =
  let exclude loc =
    try
      match String_map.find loc h.genv with
      | Global_env.FunDef _ -> true
      | _ -> false
    with Not_found -> false
  in
  Format.fprintf fmt "@[%a@]" (Mem.pp ~exclude) h.mem

let pp_by_need (_ : SS.t) fmt h = pp fmt h
let get_print_info _ _ = (SS.empty, SS.empty)

(* let str_noheap _ = "NO HEAP PRINTED" *)

let pp_branch fmt branch =
  let _, values = branch in
  Fmt.pf fmt "Returns: %a@.(Ignoring heap)" (Fmt.Dump.list Expr.pp) values

let lift_dr_and_log res =
  let pp_res = Fmt.Dump.result ~ok:pp_branch ~error:pp_err in
  Delayed.map res (fun res ->
      Logging.verbose (fun fmt -> fmt "Resulting in: %a" pp_res res);
      res)

(* Actual action execution *)

let filter_errors dr =
  Delayed.bind dr (fun res ->
      match res with
      | Ok res -> Delayed.return res
      | Error err ->
          Logging.tmi (fun m -> m "Filtering error branch: %a" pp_err err);
          Delayed.vanish ())

let consume ~core_pred heap params =
  Logging.verbose (fun m ->
      m "Executing consumer for %s with params %a" core_pred pp_params params);
  Logging.tmi (fun fmt -> fmt "Current heap : %a" pp heap);
  let open LActions in
  let a_ret =
    match ga_from_str core_pred with
    | Single -> execute_cons_single heap params
    | Array -> execute_cons_array heap params
    | Hole -> execute_cons_hole heap params
    | Zeros -> execute_cons_zeros heap params
    | Bounds -> execute_cons_bounds heap params
    | Freed -> execute_cons_freed heap params
  in
  lift_dr_and_log a_ret

let produce ~core_pred heap params =
  let open Delayed.Syntax in
  Logging.verbose (fun m ->
      m "Executing producer for %s with params %a" core_pred pp_params params);
  Logging.tmi (fun fmt -> fmt "Current heap : %a" pp heap);
  let open LActions in
  let+ a_ret =
    match ga_from_str core_pred with
    | Single -> execute_prod_single heap params |> filter_errors
    | Array -> execute_prod_array heap params |> filter_errors
    | Hole -> execute_prod_hole heap params |> filter_errors
    | Zeros -> execute_prod_zeros heap params |> filter_errors
    | Bounds -> execute_prod_bounds heap params |> filter_errors
    | Freed -> execute_prod_freed heap params
  in
  Logging.verbose (fun m -> m "Resultin in: %a" pp a_ret);
  a_ret

let execute_action ~action_name heap params =
  Logging.verbose (fun fmt ->
      fmt "Executing action %s with params %a" action_name pp_params params);
  Logging.tmi (fun fmt -> fmt "Current heap : %a" pp heap);
  let open LActions in
  let a_ret =
    match ac_from_str action_name with
    | AMem Alloc -> execute_alloc heap params
    | AMem GetCurPerm -> execute_getcurperm heap params
    | AMem WeakValidPointer -> execute_weak_valid_pointer heap params
    | AMem DropPerm -> execute_drop_perm heap params
    | AMem Store -> execute_store heap params
    | AMem Load -> execute_load heap params
    | AMem Free -> execute_free heap params
    | AMem Move -> execute_move heap params
    | AMem SetZeros ->
        DR.map (execute_prod_zeros heap params) (fun heap ->
            make_branch ~heap ())
    | AGEnv GetDef -> execute_genvgetdef heap params
  in
  lift_dr_and_log a_ret

(* LActions static *)

(* Serialization and operations *)
let substitution_in_place subst heap =
  let open Delayed.Syntax in
  let { mem; genv } = heap in
  let+ mem = Mem.substitution subst mem in
  match mem with
  | Ok mem -> { mem; genv }
  | Error e -> Fmt.failwith "Error in substitution: %a" SHeapTree.pp_err e

let clean_up ?(keep = Expr.Set.empty) _ : Expr.Set.t * Expr.Set.t =
  (Expr.Set.empty, keep)

let lvars heap = Mem.lvars heap.mem
let alocs heap = Mem.alocs heap.mem

let assertions ?to_keep:_ heap =
  let exclude loc =
    try
      match String_map.find loc heap.genv with
      | Global_env.FunDef _ -> true
      | _ -> false
    with Not_found -> false
  in
  let mem_asrts = Mem.assertions ~exclude heap.mem in
  mem_asrts

let mem_constraints _heap = []
let is_overlapping_asrt = LActions.is_overlapping_asrt_str

module Lift = struct
  open Debugger.Utils

  let get_store_vars store =
    store
    |> List.map (fun (var, value) : Variable.t ->
           let value = Fmt.to_to_string (Fmt.hbox Expr.pp) value in
           Variable.create_leaf var value ())
    |> List.sort (fun (v : Variable.t) w -> Stdlib.compare v.name w.name)

  let make_node ~get_new_scope_id ~variables ~name ~value ?(children = []) () :
      Variable.t =
    let var_ref = get_new_scope_id () in
    let () =
      match children with
      | [] -> ()
      | _ -> Hashtbl.replace variables var_ref children
    in
    let type_ =
      match children with
      | [] -> None
      | _ -> Some "object"
    in
    { name; value; var_ref; type_ }

  let add_variables
      ~store
      ~(memory : t)
      ~is_gil_file:_
      ~get_new_scope_id
      variables =
    let mem = memory.mem in
    (* Store first *)
    let store_id = get_new_scope_id () in
    let store_vars = get_store_vars store in
    Hashtbl.replace variables store_id store_vars;
    (* And finally heap*)
    let make_node = make_node ~get_new_scope_id ~variables in
    let heap_id = get_new_scope_id () in
    let heap_vars =
      Mem.SMap.to_seq mem
      |> Seq.map (fun (loc, tree) ->
             SHeapTree.Lift.get_variable ~make_node ~loc tree)
      |> List.of_seq
    in
    Hashtbl.replace variables heap_id heap_vars;
    Variable.
      [
        { id = store_id; name = "Store" };
        { id = heap_id; name = "Heap" }
        (* { id = genv_id; name = "Global Environment" }; *);
      ]
end

(** Things defined for BiAbduction *)

let get_recovery_tactic _ err =
  let values =
    match err with
    | InvalidLocation e ->
        List.map (fun x -> Expr.LVar x) (SS.elements (Expr.lvars e))
    | MissingLocResource
        { is_store = _; action = _; loc_name; ofs_opt = _; chunk_opt = _ } ->
        [ Expr.loc_from_loc_name loc_name ]
    | SHeapTreeErr { at_locations; _ } ->
        List.map Expr.loc_from_loc_name at_locations
  in
  Recovery_tactic.try_unfold values

let get_failing_constraint _e =
  failwith "Bi-abduction TODO: implement function get_failing_constraint"

let offset_by_chunk low chunk =
  let open Expr.Infix in
  let len = Expr.int (Chunk.size chunk) in
  low + len

let can_fix err =
  match err with
  | InvalidLocation _ | MissingLocResource _
  | SHeapTreeErr { sheaptree_err = MissingResource _; _ } -> true
  | _ -> false

let get_fixes err =
  let open Constr.Core in
  Logging.verbose (fun m -> m "Getting fixes for error : %a" pp_err err);
  let get_fixes_h is_store loc ofs chunk =
    let open CConstants.VTypes in
    let perm = Some Perm.Freeable in

    let fixes =
      match chunk with
      | Chunk.Mptr ->
          let new_var1 = LVar.alloc () in
          let new_var_e1 = Expr.LVar new_var1 in
          let new_var2 = LVar.alloc () in
          let new_var_e2 = Expr.LVar new_var2 in
          let value = Expr.EList [ new_var_e1; new_var_e2 ] in
          let null_typ =
            if Compcert.Archi.ptr64 then Expr.string long_type
            else Expr.string int_type
          in
          let null_ptr = Expr.EList [ null_typ; Expr.int 0 ] in
          [
            [
              single ~loc ~ofs ~chunk ~sval:value ~perm;
              Asrt.Types
                [ (new_var_e1, Type.ObjectType); (new_var_e2, Type.IntType) ];
            ];
            [ single ~loc ~ofs ~chunk ~sval:null_ptr ~perm ];
          ]
      | _ ->
          let type_str, type_gil =
            match chunk with
            | Chunk.Mfloat32 -> (single_type, Type.NumberType)
            | Chunk.Mfloat64 -> (float_type, Type.NumberType)
            | Chunk.Mint64 -> (long_type, Type.IntType)
            | _ -> (int_type, Type.IntType)
          in
          let new_var = LVar.alloc () in
          let new_var_e = Expr.LVar new_var in
          let value = Expr.EList [ Expr.string type_str; new_var_e ] in
          [
            [
              single ~loc ~ofs ~chunk ~sval:value ~perm;
              Asrt.Types [ (new_var_e, type_gil) ];
            ];
          ]
    in
    (* Additional fix for store operation to handle case of unitialized memory *)
    let fixes =
      if is_store then
        [ hole ~loc ~low:ofs ~high:(offset_by_chunk ofs chunk) ~perm ] :: fixes
      else fixes
    in
    fixes
  in
  match err with
  | MissingLocResource
      {
        is_store;
        action = Single;
        loc_name;
        ofs_opt = Some ofs;
        chunk_opt = Some chunk;
      } -> get_fixes_h is_store (Expr.loc_from_loc_name loc_name) ofs chunk
  | InvalidLocation loc ->
      let new_loc = ALoc.alloc () in
      let new_expr = Expr.ALoc new_loc in
      [ [ Asrt.Pure (BinOp (new_expr, Equal, loc)) ] ]
  | SHeapTreeErr
      {
        at_locations;
        sheaptree_err = MissingResource (Fixable { is_store; low; chunk });
      } -> (
      match at_locations with
      | [ loc ] -> get_fixes_h is_store (Expr.loc_from_loc_name loc) low chunk
      | _ ->
          Logging.verbose (fun m ->
              m "SHeapTreeErr: Unsupported for more than 1 location");
          [])
  | _ -> []

let split_further _ _ _ _ = None
