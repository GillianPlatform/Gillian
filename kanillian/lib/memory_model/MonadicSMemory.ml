module GUtils = Gillian.Utils
open GUtils.Prelude
module Result = Stdlib.Result
open Gillian.Monadic
module DR = Delayed_result
module DO = Delayed_option
open Delayed.Syntax
open Gillian.Symbolic
open Gillian.Gil_syntax
module Logging = Gillian.Logging
open SVal
module GEnv = GEnv.Symbolic

(* Some utils first *)

let resolve_or_create_loc_name (lvar_loc : Expr.t) : string Delayed.t =
  let open Delayed.Syntax in
  let* loc_name = Delayed.resolve_loc lvar_loc in
  match loc_name with
  | None ->
      let new_loc_name = ALoc.alloc () in
      let learned = [ Formula.Eq (ALoc new_loc_name, lvar_loc) ] in
      Logging.verbose (fun fmt ->
          fmt "Couldn't resolve loc %a, created %s" Expr.pp lvar_loc
            new_loc_name);
      Delayed.return ~learned new_loc_name
  | Some l ->
      Logging.verbose (fun fmt -> fmt "Resolved %a as %s" Expr.pp lvar_loc l);
      Delayed.return l

let expr_of_loc_name loc_name =
  if GUtils.Names.is_aloc_name loc_name then Expr.ALoc loc_name
  else Lit (Loc loc_name)

type init_data = unit

let get_init_data _ = ()

type vt = Values.t
type st = Subst.t

type err_t =
  | InvalidLocation of Expr.t
  | NonPositiveArraySize of Expr.t
  | MissingLocResource of string
  | SHeapTreeErr of {
      at_locations : string list;
      sheaptree_err : SHeapTree.err;
    }
  | GEnvErr of GEnv.err_t
[@@deriving show, yojson]

let lift_sheaptree_err loc (err : SHeapTree.err) =
  SHeapTreeErr { at_locations = [ loc ]; sheaptree_err = err }

let lift_genv_res r = Result.map_error (fun e -> GEnvErr e) r

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
  let copy x = x

  let get_tree_res map loc_name =
    DR.of_option ~none:(MissingLocResource loc_name)
      (SMap.find_opt loc_name map)

  let get_or_create_tree map loc_name =
    match SMap.find_opt loc_name map with
    | Some t -> Delayed.return t
    | None -> Delayed.return SHeapTree.empty

  let alloc (map : t) low high : t * string =
    let loc = ALoc.alloc () in
    let tree = SHeapTree.alloc low high in
    (SMap.add loc tree map, loc)

  let weak_valid_pointer map loc ofs =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name in
    map_lift_err loc_name (SHeapTree.weak_valid_pointer tree ofs)

  let getcurperm map loc ofs =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name in
    map_lift_err loc_name (SHeapTree.get_perm_at tree ofs)

  let drop_perm map loc low high new_perm =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name in
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.drop_perm tree low high new_perm)
    in
    SMap.add loc_name new_tree map

  let store map loc chunk ofs value =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name in
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.store tree chunk ofs value)
    in
    SMap.add loc_name new_tree map

  let load map loc chunk ofs =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name in
    let++ value, new_tree =
      map_lift_err loc_name (SHeapTree.load tree chunk ofs)
    in
    (value, SMap.add loc_name new_tree map)

  let free map loc low high =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name in
    let++ new_tree = map_lift_err loc_name (SHeapTree.free tree low high) in
    SMap.add loc_name new_tree map

  let zero_init map loc ofs size =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name in
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.zero_init tree ofs size)
    in
    SMap.add loc_name new_tree map

  let poison map loc ofs size =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name in
    let++ new_tree = map_lift_err loc_name (SHeapTree.poison tree ofs size) in
    SMap.add loc_name new_tree map

  let cons_single map loc ofs chunk =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name in
    let++ sval, perm, new_tree =
      map_lift_err loc_name (SHeapTree.cons_single tree ofs chunk)
    in
    (SMap.add loc_name new_tree map, sval, perm)

  let prod_single map loc ofs chunk sval perm =
    let open DR.Syntax in
    let* loc_name = resolve_or_create_loc_name loc in
    let* tree = get_or_create_tree map loc_name in
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.prod_single tree ofs chunk sval perm)
    in
    SMap.add loc_name new_tree map

  let get_array map loc ofs size chunk =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in

    let open Formula.Infix in
    if%sat size #<= (Expr.int 0) then DR.error (NonPositiveArraySize size)
    else
      let** tree = get_tree_res map loc_name in
      let++ sarr, perm, new_tree =
        map_lift_err loc_name (SHeapTree.get_array tree ofs size chunk)
      in
      (SMap.add loc_name new_tree map, loc_name, sarr, perm)

  let cons_array map loc ofs size chunk =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in

    let open Formula.Infix in
    if%sat size #<= (Expr.int 0) then DR.error (NonPositiveArraySize size)
    else
      let** tree = get_tree_res map loc_name in
      let++ sarr, perm, new_tree =
        map_lift_err loc_name (SHeapTree.cons_array tree ofs size chunk)
      in
      (SMap.add loc_name new_tree map, loc_name, sarr, perm)

  let prod_array map loc ofs size chunk array perm =
    let open DR.Syntax in
    let open Formula.Infix in
    if%sat size #<= (Expr.int 0) then DR.ok map
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
    let** tree = get_tree_res map loc_name in
    let++ () =
      DR.of_result (SHeapTree.get_freed tree) |> map_lift_err loc_name
    in
    SMap.remove loc_name map

  let prod_freed map loc =
    let+ loc_name = resolve_or_create_loc_name loc in
    SMap.add loc_name SHeapTree.freed map

  let cons_simple ~sheap_consumer map loc low high =
    let open DR.Syntax in
    let open Formula.Infix in
    let** loc_name = resolve_loc_result loc in
    if%sat high #<= low then DR.ok (map, Some Perm.Freeable)
    else
      let** tree = get_tree_res map loc_name in
      let++ new_tree, perm =
        map_lift_err loc_name (sheap_consumer tree low high)
      in
      (SMap.add loc_name new_tree map, perm)

  let prod_simple ~sheap_producer map loc low high perm =
    let open DR.Syntax in
    let open Formula.Infix in
    if%sat high #<= low then DR.ok map
    else
      let* loc_name = resolve_or_create_loc_name loc in
      let* tree = get_or_create_tree map loc_name in
      let++ new_tree =
        map_lift_err loc_name (sheap_producer tree low high perm)
      in
      SMap.add loc_name new_tree map

  let cons_hole = cons_simple ~sheap_consumer:SHeapTree.cons_hole
  let prod_hole = prod_simple ~sheap_producer:SHeapTree.prod_hole
  let cons_zeros = cons_simple ~sheap_consumer:SHeapTree.cons_zeros
  let prod_zeros = prod_simple ~sheap_producer:SHeapTree.prod_zeros

  let get_bounds map loc =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name in
    let++ bounds =
      map_lift_err loc_name (DR.of_result (SHeapTree.load_bounds tree))
    in
    bounds

  let cons_bounds map loc =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name in
    let++ bounds, new_tree =
      map_lift_err loc_name (DR.of_result (SHeapTree.cons_bounds tree))
    in
    (bounds, SMap.add loc_name new_tree map)

  let prod_bounds map loc bounds =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let* tree = get_or_create_tree map loc_name in
    let++ tree_set =
      map_lift_err loc_name (DR.of_result (SHeapTree.prod_bounds tree bounds))
    in
    SMap.add loc_name tree_set map

  let move map dst_loc dst_ofs src_loc src_ofs sz =
    let open DR.Syntax in
    let open Formula.Infix in
    if%sat sz #== (Expr.int 0) then DR.ok map
    else
      let** dst_loc_name = resolve_loc_result dst_loc in
      let** src_loc_name = resolve_loc_result src_loc in
      let** dst_tree = get_tree_res map dst_loc_name in
      let** src_tree = get_tree_res map src_loc_name in
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
        if not (List.mem loc exclude) then SHeapTree.assertions ~loc tree @ acc
        else acc)
      map []

  let pp_full ~genv ft mem =
    let is_fun loc =
      try
        match GEnv.find_def genv loc with
        | FunDef _ -> true
        | _ -> false
      with Not_found -> false
    in
    let iter_exclude f map =
      SMap.iter (fun loc x -> if not (is_fun loc) then f loc x) map
    in
    let open Fmt in
    pf ft "%a"
      (Dump.iter_bindings iter_exclude nop string SHeapTree.pp_full)
      mem

  let substitution ?(genv = GEnv.empty) subst mem : (t, SHeapTree.err) DR.t =
    let open DR.Syntax in
    if Subst.domain subst None = Expr.Set.empty then DR.ok mem
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
      let svarr_subst = SVArray.subst ~le_subst in
      let subst_tree =
        SHeapTree.substitution ~le_subst ~sval_subst ~svarr_subst
      in
      let substituted = SMap.map subst_tree mem in

      List.fold_left
        (fun acc (old_loc, new_loc) ->
          let** acc = acc in
          Logging.verbose (fun fmt ->
              fmt "SHOULD Merge locs: %s --> %a" old_loc Expr.pp new_loc);
          Logging.tmi (fun fmt -> fmt "IN MEMORY: %a" (pp_full ~genv) acc);
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

  let pp_normal ~genv ft mem =
    let is_fun loc =
      try
        match GEnv.find_def genv loc with
        | FunDef _ -> true
        | _ -> false
      with Not_found -> false
    in
    let is_first = ref true in
    SMap.iter
      (fun loc tree ->
        if not (is_fun loc) then (
          if !is_first then is_first := false else Fmt.pf ft "@\n";
          Fmt.pf ft "%s -> @[<v 0>%a@]" loc SHeapTree.pp tree))
      mem

  let pp ?(genv = GEnv.empty) fmt map = pp_normal ~genv fmt map
end

type t = { genv : GEnv.t; mem : Mem.t }

let to_yojson t =
  let { genv = _; mem } = t in
  Mem.to_yojson mem

let of_yojson m =
  Result.map (fun mem -> { genv = GEnv.empty; mem }) (Mem.of_yojson m)

type action_ret = (t * vt list, err_t) result

let make_branch ~heap ?(rets = []) () = (heap, rets)

(* Init *)

let init () = { genv = GEnv.empty; mem = Mem.empty }
let clear h = { h with mem = Mem.empty }
let copy h = h

let sure_is_nonempty _ =
  (* TODO: Implementing this would require filtering functions
           from the global environment. We over-approximate by returning false *)
  false

let split_further _ _ _ _ = None

(* let subst_spec_vars _ _ = () *)

(* Ungraceful failure *)

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
      let perm_string = Expr.Lit (String (Perm.opt_to_string perm)) in
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
      let perm = Perm.of_string perm_string in
      let++ mem = Mem.drop_perm heap.mem loc low high perm in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully "drop_perm" params

let execute_store heap params =
  let open DR.Syntax in
  match params with
  | [ Expr.Lit (String chunk_name); loc; ofs; value ] ->
      let chunk = Chunk.of_string chunk_name in
      let sval = SVal.make ~value ~chunk in
      let++ mem = Mem.store heap.mem loc chunk ofs sval in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully "store" params

let execute_load heap params =
  let open DR.Syntax in
  match params with
  | [ Expr.Lit (String chunk_name); loc; ofs ] ->
      let chunk = Chunk.of_string chunk_name in
      let** value, mem = Mem.load heap.mem loc chunk ofs in
      let gil_value = SVal.to_gil_expr ~chunk value in
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

let execute_zero_init heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; size ] ->
      let++ mem = Mem.zero_init heap.mem loc ofs size in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully "store" params

let execute_poison heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; size ] ->
      let++ mem = Mem.poison heap.mem loc ofs size in
      make_branch ~heap:{ heap with mem } ~rets:[] ()
  | _ -> fail_ungracefully "store" params

let execute_cons_single heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; Expr.Lit (String chunk_string) ] ->
      let chunk = Chunk.of_string chunk_string in
      let** mem, sval, perm = Mem.cons_single heap.mem loc ofs chunk in
      let sval_e = SVal.to_gil_expr ~chunk sval in
      let perm_string = Perm.opt_to_string perm in
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
   value;
   Expr.Lit (String perm_string);
  ] ->
      let perm = Perm.of_string perm_string in
      let chunk = Chunk.of_string chunk_string in
      let sval = SVal.make ~chunk ~value in
      let++ mem = Mem.prod_single heap.mem loc ofs chunk sval perm in
      { heap with mem }
  | _ -> fail_ungracefully "set_single" params

let execute_cons_array heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; size; Expr.Lit (String chunk_string) ] ->
      let chunk = Chunk.of_string chunk_string in
      let** mem, loc_name, array, perm =
        Mem.cons_array heap.mem loc ofs size chunk
      in
      let array_e = SVArray.to_gil_expr ~chunk ~size array in
      let perm_string = Perm.opt_to_string perm in
      DR.ok
        (make_branch ~heap:{ heap with mem }
           ~rets:[ array_e; Expr.Lit (String perm_string) ]
           ())
  | _ -> fail_ungracefully "cons_array" params

let execute_get_array heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; size; Expr.Lit (String chunk_string) ] ->
      let chunk = Chunk.of_string chunk_string in
      let** mem, loc_name, array, perm =
        Mem.get_array heap.mem loc ofs size chunk
      in
      let loc_e = expr_of_loc_name loc_name in
      let array_e = SVArray.to_gil_expr ~chunk ~size array in
      let perm_string = Perm.opt_to_string perm in
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

let execute_prod_array heap params =
  let open DR.Syntax in
  match params with
  | [
   loc;
   ofs;
   size;
   Expr.Lit (String chunk_string);
   values;
   Expr.Lit (String perm_string);
  ] ->
      let perm = Perm.of_string perm_string in
      let chunk = Chunk.of_string chunk_string in
      let arr = SVArray.make ~chunk ~values in
      let++ mem = Mem.prod_array heap.mem loc ofs size chunk arr perm in
      { heap with mem }
  | _ -> fail_ungracefully "set_single" params

let execute_cons_simple ~mem_consumer ~name heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high ] ->
      let** mem, perm = mem_consumer heap.mem loc low high in
      let perm_e = Expr.string (Perm.opt_to_string perm) in
      DR.ok (make_branch ~heap:{ heap with mem } ~rets:[ perm_e ] ())
  | _ -> fail_ungracefully name params

let execute_prod_simple ~mem_producer ~name heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high; Expr.Lit (String perm_string) ] ->
      let perm = Perm.of_string perm_string in
      let++ mem = mem_producer heap.mem loc low high perm in
      { heap with mem }
  | _ -> fail_ungracefully name params

let execute_cons_hole =
  execute_cons_simple ~mem_consumer:Mem.cons_hole ~name:"cons_hole"

let execute_prod_hole =
  execute_prod_simple ~mem_producer:Mem.prod_hole ~name:"prod_hole"

let execute_cons_zeros =
  execute_cons_simple ~mem_consumer:Mem.cons_zeros ~name:"cons_zeros"

let execute_prod_zeros =
  execute_prod_simple ~mem_producer:Mem.prod_zeros ~name:"prod_zeros"

let execute_cons_freed heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let++ mem = Mem.cons_freed heap.mem loc in
      make_branch ~heap ~rets:[] ()
  | _ -> fail_ungracefully "cons_freed" params

let execute_prod_freed heap params =
  match params with
  | [ loc ] ->
      let+ mem = Mem.prod_freed heap.mem loc in
      { heap with mem }
  | _ -> fail_ungracefully "prod_freed" params

let execute_get_bounds heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let++ low, high = Mem.get_bounds heap.mem loc in
      let bounds_e = Expr.EList [ low; high ] in
      (* The fact that this returns a loc is legacy.
         It's not necessary, but changing that means changing the compiler and runtime *)
      make_branch ~heap ~rets:[ loc; bounds_e ] ()
  | _ -> fail_ungracefully "get_bounds" params

let execute_cons_bounds heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let++ (low, high), mem = Mem.cons_bounds heap.mem loc in
      let bounds_e = Expr.EList [ low; high ] in
      make_branch ~heap:{ heap with mem } ~rets:[ bounds_e ] ()
  | _ -> fail_ungracefully "cons_bounds" params

let execute_prod_bounds heap params =
  let open DR.Syntax in
  match params with
  | [ loc; bounds_e ] ->
      let bounds =
        match bounds_e with
        | Expr.EList [ low; high ] -> (low, high)
        | Lit (LList [ low; high ]) -> (Lit low, Lit high)
        | _ -> fail_ungracefully "prod_bounds" params
      in
      let++ mem = Mem.prod_bounds heap.mem loc bounds in
      { heap with mem }
  | _ -> fail_ungracefully "prod_bounds" params

let execute_genvgetsymbol heap params =
  let ( let- ) x f = Result.map f (lift_genv_res x) in
  let open Gillian.Gil_syntax.Expr in
  match params with
  | [ Lit (String symbol) ] ->
      Delayed.return
      @@ let- sym = GEnv.find_symbol heap.genv symbol in
         make_branch ~heap
           ~rets:[ Lit (String symbol); loc_from_loc_name sym ]
           ()
  | _ -> fail_ungracefully "genv_getsymbol" params

let execute_cons_symbol heap params =
  let ( let- ) x f = Result.map f (lift_genv_res x) in
  let open Gillian.Gil_syntax.Expr in
  match params with
  | [ Lit (String symbol) ] ->
      Delayed.return
      @@ let- sym = GEnv.find_symbol heap.genv symbol in
         make_branch ~heap ~rets:[ loc_from_loc_name sym ] ()
  | _ -> fail_ungracefully "genv_getsymbol" params

let execute_genvsetsymbol heap params =
  match params with
  | [ Expr.Lit (String symbol); lvar_loc ] ->
      let+ genv = GEnv.set_symbol heap.genv symbol lvar_loc in
      Ok (make_branch ~heap:{ heap with genv } ~rets:[] ())
  | _ -> fail_ungracefully "genv_getsymbol" params

let execute_prod_symbol heap params =
  match params with
  | [ Expr.Lit (String symbol); lvar_loc ] ->
      let+ genv = GEnv.set_symbol heap.genv symbol lvar_loc in
      { heap with genv }
  | _ -> fail_ungracefully "genv_getsymbol" params

let execute_cons_definition heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let** loc_name = resolve_loc_result loc in
      let def = GEnv.find_def heap.genv loc_name in
      let v = GEnv.serialize_def def in
      DR.ok (make_branch ~heap ~rets:[ v ] ())
  | _ -> fail_ungracefully "genv_getdef" params

let execute_genvgetdef heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let** loc_name = resolve_loc_result loc in
      let def = GEnv.find_def heap.genv loc_name in
      let v = GEnv.serialize_def def in
      DR.ok (make_branch ~heap ~rets:[ Expr.loc_from_loc_name loc_name; v ] ())
  | _ -> fail_ungracefully "genv_getdef" params

let execute_prod_definition heap params =
  match params with
  | [ lvar_loc; v_def ] ->
      let* loc_name = resolve_or_create_loc_name lvar_loc in
      let def = GEnv.deserialize_def v_def in
      let+ genv = GEnv.set_def heap.genv loc_name def in
      { heap with genv }
  | _ -> fail_ungracefully "genv_setdef" params

let execute_genvsetdef heap params =
  match params with
  | [ lvar_loc; v_def ] ->
      let* loc_name = resolve_or_create_loc_name lvar_loc in
      let def = GEnv.deserialize_def v_def in
      let* genv = GEnv.set_def heap.genv loc_name def in
      DR.ok (make_branch ~heap:{ heap with genv } ())
  | _ -> fail_ungracefully "genv_setdef" params

(* Pretty printing utils *)

let pp_err fmt (e : err_t) =
  match e with
  | InvalidLocation loc ->
      Fmt.pf fmt "'%a' cannot be resolved as a location" Expr.pp loc
  | MissingLocResource l ->
      Fmt.pf fmt "No block associated with location '%s'" l
  | SHeapTreeErr { at_locations; sheaptree_err } ->
      Fmt.pf fmt "Tree at location%a raised: <%a>"
        (fun fmt l ->
          match l with
          | [ s ] -> Fmt.pf fmt " '%s'" s
          | l -> Fmt.pf fmt "s %a" (Fmt.Dump.list Fmt.string) l)
        at_locations SHeapTree.pp_err sheaptree_err
  | GEnvErr (Symbol_not_found s) -> Fmt.pf fmt "Symbol not found: %s" s
  | NonPositiveArraySize vt ->
      Fmt.pf fmt "Array sizes should be strictly positive: %a" Expr.pp vt

(* let str_of_err e = Format.asprintf "%a" pp_err e *)

let pp fmt h =
  Format.fprintf fmt "GEnv : HIDDEN@\nMem  : @[%a@]"
    (* GEnv.pp !h.genv *)
    (* No need to print the genv really, it's just polluting... *)
    (Mem.pp ~genv:h.genv)
    h.mem

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

let filter_errors dr =
  Delayed.bind dr (fun res ->
      match res with
      | Ok res -> Delayed.return res
      | Error err ->
          Logging.tmi (fun m -> m "Filtering error branch: %a" pp_err err);
          Delayed.vanish ())
(* Actual action execution *)

let consume ~core_pred heap params =
  Logging.verbose (fun m ->
      m "Executing consumer for %s with params %a" core_pred pp_params params);
  Logging.tmi (fun fmt -> fmt "Current heap : %a" pp heap);
  let open Interface in
  let a_ret =
    match ga_from_str core_pred with
    | GMem Single -> execute_cons_single heap params
    | GMem Array -> execute_cons_array heap params
    | GMem Hole -> execute_cons_hole heap params
    | GMem Zeros -> execute_cons_zeros heap params
    | GMem Freed -> execute_cons_freed heap params
    | GMem Bounds -> execute_cons_bounds heap params
    | GGenv Definition -> execute_cons_definition heap params
    | GGenv Symbol -> execute_cons_symbol heap params
  in
  lift_dr_and_log a_ret

let produce ~core_pred heap params =
  let open Delayed.Syntax in
  Logging.verbose (fun m ->
      m "Executing producer for %s with params %a" core_pred pp_params params);
  Logging.tmi (fun fmt -> fmt "Current heap : %a" pp heap);
  let open Interface in
  let+ a_ret =
    match ga_from_str core_pred with
    | GMem Single -> execute_prod_single heap params |> filter_errors
    | GMem Array -> execute_prod_array heap params |> filter_errors
    | GMem Hole -> execute_prod_hole heap params |> filter_errors
    | GMem Zeros -> execute_prod_zeros heap params |> filter_errors
    | GMem Bounds -> execute_prod_bounds heap params |> filter_errors
    | GMem Freed -> execute_prod_freed heap params
    | GGenv Definition -> execute_prod_definition heap params
    | GGenv Symbol -> execute_prod_symbol heap params
  in
  Logging.verbose (fun m -> m "Resultin in: %a" pp a_ret);
  a_ret

let execute_action ~action_name heap params =
  Logging.verbose (fun fmt ->
      fmt "Executing action %s with params %a" action_name pp_params params);
  Logging.tmi (fun fmt -> fmt "Current heap : %a" pp heap);
  let open Interface in
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
    | AMem ZeroInit -> execute_zero_init heap params
    | AMem Poison -> execute_poison heap params
    | AMem GetArray -> execute_get_array heap params
    | AMem GetBounds -> execute_get_bounds heap params
    | AGEnv GetSymbol -> execute_genvgetsymbol heap params
    | AGEnv SetSymbol -> execute_genvsetsymbol heap params
    | AGEnv GetDef -> execute_genvgetdef heap params
    | AGEnv SetDef -> execute_genvsetdef heap params
  in
  lift_dr_and_log a_ret

(* Interface static *)

(* Serialization and operations *)
let substitution_in_place subst heap =
  let open Delayed.Syntax in
  let { mem; genv } = heap in
  let genv = GEnv.substitution subst genv in
  let+ mem = Mem.substitution ~genv subst mem in
  match mem with
  | Ok mem -> { mem; genv }
  | Error e -> Fmt.failwith "Error in substitution: %a" SHeapTree.pp_err e

(* let { mem; genv } = !heap in
   let nmem = Mem.substitution subst mem in
   let ngenv = GEnv.substitution subst genv in
   heap := { mem = nmem; genv = ngenv } *)

let fresh_val _ = Expr.LVar (LVar.alloc ())

let clean_up ?(keep = Expr.Set.empty) _ : Expr.Set.t * Expr.Set.t =
  (Expr.Set.empty, keep)

let lvars heap = Mem.lvars heap.mem
let alocs heap = Mem.alocs heap.mem

let assertions ?to_keep:_ heap =
  let genv_locs, genv_asrts = GEnv.assertions heap.genv in
  let mem_asrts = Mem.assertions ~exclude:genv_locs heap.mem in
  genv_asrts @ mem_asrts

(* let genv_locs, genv_asrts = GEnv.assertions !heap.genv in
   let mem_asrts = Mem.assertions ~exclude:genv_locs !heap.mem in
   genv_asrts @ mem_asrts *)

let mem_constraints _heap = []
let is_overlapping_asrt = Interface.is_overlapping_asrt_str

module Lift = struct
  open Gillian.Debugger
  open Gillian.Debugger.Utils

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

  let add_variables ~store ~memory ~is_gil_file:_ ~get_new_scope_id variables =
    let { mem; genv = _ } = memory in
    (* Store first *)
    let store_id = get_new_scope_id () in
    let store_vars = get_store_vars store in
    Hashtbl.replace variables store_id store_vars;
    (* Then genv *)
    (* TODO: Print the global environment *)
    (* let genv_id = get_new_scope_id () in
       let genv_vars = GEnv.Lifting.get_vars genv in
       Hashtbl.replace variables genv_id genv_vars; *)
    (* And finally heap*)
    let make_node = make_node ~get_new_scope_id ~variables in
    let heap_id = get_new_scope_id () in
    let heap_vars =
      Mem.SMap.to_seq mem
      |> Seq.map (fun (loc, tree) ->
             SHeapTree.Lift.get_variable ~make_node ~loc tree)
      |> List.of_seq
    in
    Debugger_log.log (fun fmt ->
        fmt "There are %d heap vars" (List.length heap_vars));
    Hashtbl.replace variables heap_id heap_vars;
    Variable.
      [
        { id = store_id; name = "Store" };
        { id = heap_id; name = "Heap" }
        (* { id = genv_id; name = "Global Environment" }; *);
      ]
end

(** Things defined for BiAbduction *)

let get_recovery_vals _ = function
  | InvalidLocation e ->
      List.map (fun x -> Expr.LVar x) (SS.elements (Expr.lvars e))
  | MissingLocResource l -> [ Expr.loc_from_loc_name l ]
  | SHeapTreeErr { at_locations; _ } ->
      List.map Expr.loc_from_loc_name at_locations
  | GEnvErr (Symbol_not_found s) -> [ Expr.string s ]
  | NonPositiveArraySize e -> [ e ]

let get_recovery_tactic _ err =
  Gillian.General.Recovery_tactic.try_unfold (get_recovery_vals () err)

let get_failing_constraint e =
  Fmt.failwith "Not ready for bi-abduction yet: get_failing_constraint %a"
    pp_err e

let get_fixes err = Fmt.failwith "unimplemented get_fix for %a" pp_err err

let can_fix = function
  | MissingLocResource _
  | SHeapTreeErr { sheaptree_err = MissingResource; _ }
  | InvalidLocation _ -> true
  | _ -> false
