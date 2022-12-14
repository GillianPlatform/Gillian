(***** This module defines a Wisl Symbolic Heap *******)
open Gillian.Symbolic
open Gillian.Gil_syntax
module Solver = Gillian.Logic.FOSolver
module Reduction = Gillian.Logic.Reduction
open Gillian.Debugger.Utils

type err =
  | MissingResource of (WislLActions.ga * string * Expr.t option)
  | DoubleFree of string
  | UseAfterFree of string
  | MemoryLeak
  | OutOfBounds of (int option * string * Expr.t)
  | InvalidLocation
[@@deriving yojson, show]

module Block = struct
  type t = Freed | Allocated of { data : SFVL.t; bound : int option }
  [@@deriving yojson]

  let empty = Allocated { data = SFVL.empty; bound = None }

  let substitution ~partial subst block =
    match block with
    | Freed -> Freed
    | Allocated { data; bound } ->
        let data = SFVL.substitution subst partial data in
        Allocated { data; bound }

  let assertions ~loc block =
    let eloc = Expr.loc_from_loc_name loc in
    match block with
    | Freed -> [ Constr.freed ~loc:eloc ]
    | Allocated { data; bound } ->
        let data_asrts =
          SFVL.assertions_with_constructor
            ~constr:(fun loc offset value -> Constr.cell ~loc ~offset ~value)
            eloc data
        in
        let bound_asrt =
          match bound with
          | None -> []
          | Some bound -> [ Constr.bound ~loc:eloc ~bound ]
        in
        bound_asrt @ data_asrts

  let pp ~loc fmt block =
    match block with
    | Freed -> Fmt.pf fmt "%s -> FREED" loc
    | Allocated { data; bound } ->
        Fmt.pf fmt "%s -> @[<v>BOUND: %a@ %a@]" loc
          (Fmt.option ~none:(Fmt.any "NONE") Fmt.int)
          bound
          (Fmt.braces @@ Fmt.vbox
          @@ Fmt.iter_bindings ~sep:Fmt.sp SFVL.iter
          @@ fun ft (o, v) -> Fmt.pf ft "%a: %a" Expr.pp o Expr.pp v)
          data

  let lvars block =
    match block with
    | Freed -> SS.empty
    | Allocated { data; _ } -> SFVL.lvars data

  let alocs block =
    match block with
    | Freed -> SS.empty
    | Allocated { data; _ } -> SFVL.alocs data
end

type t = (string, Block.t) Hashtbl.t [@@deriving yojson]

(* A symbolic heap is a map from location and offset to symbolic values *)

let init () = Hashtbl.create 1

(* Simply initializes an empty heap *)

(****** Standard stuff about hashtbls ********)

let copy heap = Hashtbl.copy heap

(****** Types and functions for logging when blocks have been freed ********)

type set_freed_info = { loc : string } [@@deriving yojson]

let set_freed_info_pp fmt set_freed =
  Fmt.pf fmt "Set Freed at location %s" set_freed.loc

let set_freed_with_logging heap loc =
  let set_freed_info = { loc } in
  let _ =
    Logging.Specific.normal
      (Logging.Loggable.make set_freed_info_pp set_freed_info_of_yojson
         set_freed_info_to_yojson set_freed_info)
      Logging.LoggingConstants.ContentType.set_freed_info
  in
  Hashtbl.replace heap loc Block.Freed

(***** Implementation of local actions *****)

let alloc (heap : t) size =
  let loc = ALoc.alloc () in
  let rec get_list current_offset =
    if current_offset < 0 then []
    else
      (Expr.int current_offset, Expr.Lit Literal.Null)
      :: get_list (current_offset - 1)
  in
  let l = get_list (size - 1) in
  let sfvl = SFVL.of_list l in
  let block = Block.Allocated { data = sfvl; bound = Some size } in
  let () = Hashtbl.replace heap loc block in
  loc

let dispose (heap : t) loc =
  match Hashtbl.find_opt heap loc with
  | None -> Error (MissingResource (Cell, loc, None))
  | Some (Allocated { data = _; bound = None }) ->
      Error (MissingResource (Bound, loc, None))
  | Some Freed -> Error (DoubleFree loc)
  | Some (Allocated { data; bound = Some i }) ->
      let has_all =
        let so_far = ref true in
        for i = 0 to i - 1 do
          so_far := !so_far && (Option.is_some @@ SFVL.get (Expr.int i) data)
        done;
        !so_far
      in
      if has_all then
        let () = set_freed_with_logging heap loc in
        Ok ()
      else Error (MissingResource (Bound, loc, None))

let get_cell ~pfs ~gamma heap loc ofs =
  match Hashtbl.find_opt heap loc with
  | None -> Error (MissingResource (Cell, loc, Some ofs))
  | Some Block.Freed -> Error (UseAfterFree loc)
  | Some (Allocated { data; bound }) -> (
      let maybe_out_of_bound =
        match bound with
        | None -> false
        | Some n ->
            let n = Expr.int n in
            let open Formula.Infix in
            Solver.sat ~unification:false ~pfs ~gamma [ n #<= ofs ]
      in
      if maybe_out_of_bound then Error (OutOfBounds (bound, loc, ofs))
      else
        match SFVL.get ofs data with
        | Some v -> Ok (loc, ofs, v)
        | None -> (
            match
              SFVL.get_first
                (fun name -> Solver.is_equal ~pfs ~gamma name ofs)
                data
            with
            | Some (o, v) -> Ok (loc, o, v)
            | None -> Error (MissingResource (Cell, loc, Some ofs))))

let set_cell ~pfs ~gamma heap loc_name ofs v =
  match Hashtbl.find_opt heap loc_name with
  | None ->
      let data = SFVL.add ofs v SFVL.empty in
      let bound = None in
      let () = Hashtbl.replace heap loc_name (Allocated { data; bound }) in
      Ok ()
  | Some Block.Freed -> Error (UseAfterFree loc_name)
  | Some (Allocated { data; bound }) ->
      let maybe_out_of_bound =
        match bound with
        | None -> false
        | Some n ->
            let n = Expr.int n in
            let open Formula.Infix in
            Solver.sat ~unification:false ~pfs ~gamma [ n #<= ofs ]
      in
      if maybe_out_of_bound then Error (UseAfterFree loc_name)
      else
        let equality_test = Solver.is_equal ~pfs ~gamma in
        let data = SFVL.add_with_test ~equality_test ofs v data in
        let () = Hashtbl.replace heap loc_name (Allocated { data; bound }) in
        Ok ()

let rem_cell heap loc offset =
  match Hashtbl.find_opt heap loc with
  | None -> Error (MissingResource (Cell, loc, Some offset))
  | Some Block.Freed -> Error (UseAfterFree loc)
  | Some (Allocated { bound; data }) ->
      let data = SFVL.remove offset data in
      let () = Hashtbl.replace heap loc (Allocated { bound; data }) in
      Ok ()

let get_bound heap loc =
  match Hashtbl.find_opt heap loc with
  | Some Block.Freed -> Error (UseAfterFree loc)
  | None -> Error (MissingResource (Cell, loc, None))
  | Some (Allocated { bound = None; _ }) ->
      Error (MissingResource (Bound, loc, None))
  | Some (Allocated { bound = Some bound; _ }) -> Ok bound

let set_bound heap loc bound =
  let prev = Option.value ~default:Block.empty (Hashtbl.find_opt heap loc) in
  match prev with
  | Freed -> Error (UseAfterFree loc)
  | Allocated { data; _ } ->
      let changed = Block.Allocated { data; bound = Some bound } in
      let () = Hashtbl.replace heap loc changed in
      Ok ()

let rem_bound heap loc =
  match Hashtbl.find_opt heap loc with
  | Some Block.Freed -> Error (UseAfterFree loc)
  | None -> Error (MissingResource (Cell, loc, None))
  | Some (Allocated { bound = None; _ }) ->
      Error (MissingResource (Bound, loc, None))
  | Some (Allocated { data; _ }) ->
      let () = Hashtbl.replace heap loc (Allocated { data; bound = None }) in
      Ok ()

let get_freed heap loc =
  match Hashtbl.find_opt heap loc with
  | Some Block.Freed -> Ok ()
  | Some _ -> Error MemoryLeak
  | None -> Error (MissingResource (Freed, loc, None))

let set_freed heap loc = set_freed_with_logging heap loc

let rem_freed heap loc =
  match Hashtbl.find_opt heap loc with
  | Some Block.Freed ->
      Hashtbl.remove heap loc;
      Ok ()
  | None -> Error (MissingResource (Freed, loc, None))
  | Some _ -> Error MemoryLeak

(***** Some things specific to symbolic heaps ********)

let merge_loc (heap : t) new_loc old_loc : unit =
  let old_block, new_block =
    (Hashtbl.find_opt heap old_loc, Hashtbl.find_opt heap new_loc)
  in
  match (old_block, new_block) with
  | Some Block.Freed, Some Block.Freed -> Hashtbl.remove heap old_loc
  | None, Some Block.Freed -> ()
  | Some Block.Freed, None ->
      Hashtbl.replace heap new_loc Block.Freed;
      Hashtbl.remove heap old_loc
  | _, _ -> (
      let old_block = Option.value ~default:Block.empty old_block in
      let new_block = Option.value ~default:Block.empty new_block in
      match (old_block, new_block) with
      | _, Freed | Freed, _ -> failwith "merging non-freed and freed block"
      | ( Allocated { data = old_data; bound = old_bound },
          Allocated { data = new_data; bound = new_bound } ) ->
          let data = SFVL.union new_data old_data in
          let bound =
            if Option.is_some new_bound then new_bound else old_bound
          in
          let () = Hashtbl.replace heap new_loc (Allocated { data; bound }) in
          Hashtbl.remove heap old_loc)

let substitution_in_place subst heap :
    (t * Formula.Set.t * (string * Type.t) list) list =
  (* First we replace in the offset and values using fvl *)
  let () =
    Hashtbl.iter
      (fun loc block ->
        Hashtbl.replace heap loc (Block.substitution ~partial:true subst block))
      heap
  in
  (* Then we replace within the locations themselves *)
  let aloc_subst =
    Subst.filter subst (fun var _ ->
        match var with
        | ALoc _ -> true
        | _ -> false)
  in
  Subst.iter aloc_subst (fun aloc new_loc ->
      let aloc =
        match aloc with
        | ALoc loc -> loc
        | _ -> raise (Failure "Impossible by construction")
      in
      let new_loc_str =
        match new_loc with
        | Expr.Lit (Literal.Loc loc) -> loc
        | Expr.ALoc loc -> loc
        | _ ->
            raise
              (Failure
                 (Printf.sprintf "Heap substitution fail for loc: %s"
                    ((WPrettyUtils.to_str Expr.pp) new_loc)))
      in
      merge_loc heap new_loc_str aloc);
  [ (heap, Formula.Set.empty, []) ]

let assertions heap =
  Hashtbl.fold (fun loc block acc -> Block.assertions ~loc block @ acc) heap []

let lvars heap : SS.t =
  Hashtbl.fold
    (fun _ block acc -> SS.union (Block.lvars block) acc)
    heap SS.empty

let alocs heap : SS.t =
  Hashtbl.fold
    (fun loc block acc ->
      SS.union
        (SS.union (Block.alocs block) acc)
        (match Gillian.Utils.Names.is_aloc_name loc with
        | true -> SS.singleton loc
        | false -> SS.empty))
    heap SS.empty

(***** small things useful for printing ******)

let pp fmt heap =
  Fmt.pf fmt "@[<v>%a@]"
    ( Fmt.iter_bindings ~sep:(Fmt.any "@\n@\n") Hashtbl.iter @@ fun ft (l, b) ->
      Block.pp ~loc:l ft b )
    heap

let get_store_vars store is_gil_file =
  List.filter_map
    (fun (var, (value : Gil_syntax.Expr.t)) ->
      if (not is_gil_file) && Str.string_match (Str.regexp "gvar") var 0 then
        None
      else
        let match_offset lst loc loc_pp =
          match lst with
          | [ Expr.Lit (Int offset) ] ->
              Fmt.str "-> (%a, %d)" (Fmt.hbox loc_pp) loc (Z.to_int offset)
          | [ offset ] ->
              Fmt.str "-> (%a, %a)" (Fmt.hbox loc_pp) loc (Fmt.hbox Expr.pp)
                offset
          | _ -> Fmt.to_to_string (Fmt.hbox Expr.pp) value
        in
        let value =
          match value with
          | Expr.EList (Lit (Loc loc) :: rest) | Expr.EList (LVar loc :: rest)
            -> match_offset rest loc Fmt.string
          | _ -> Fmt.to_to_string (Fmt.hbox Expr.pp) value
        in
        Some ({ name = var; value; type_ = None; var_ref = 0 } : Variable.t))
    store
  |> List.sort Stdlib.compare

let add_memory_vars (smemory : t) (get_new_scope_id : unit -> int) variables :
    Variable.t list =
  let vstr = Fmt.to_to_string (Fmt.hbox Expr.pp) in
  let compare_offsets (v, _) (w, _) =
    try
      let open Expr.Infix in
      let difference = v - w in
      match difference with
      | Expr.Lit (Int f) ->
          if Z.lt f Z.zero then -1 else if Z.gt f Z.zero then 1 else 0
      | _ -> 0
    with _ -> (* Do not sort the offsets if an exception has occurred *)
              0
  in
  let cell_vars l : Variable.t list =
    List.sort compare_offsets l
    |> List.map (fun (offset, value) : Variable.t ->
           (* Display offset as a number to match the printing of WISL pointers *)
           let offset_str =
             match offset with
             | Expr.Lit (Int o) -> Z.to_string o
             | other -> vstr other
           in
           Variable.create_leaf offset_str (vstr value) ())
  in
  smemory |> Hashtbl.to_seq
  |> Seq.map (fun (loc, blocks) ->
         match blocks with
         | Block.Freed -> Variable.create_leaf loc "freed" ()
         | Allocated { data; bound } ->
             let bound =
               match bound with
               | None -> "none"
               | Some bound -> string_of_int bound
             in
             let bound = Variable.create_leaf "bound" bound () in
             let cells_id = get_new_scope_id () in
             let () =
               Hashtbl.replace variables cells_id
                 (cell_vars (SFVL.to_list data))
             in
             let cells = Variable.create_node "cells" cells_id () in
             let loc_id = get_new_scope_id () in
             let () = Hashtbl.replace variables loc_id [ bound; cells ] in
             Variable.create_node loc loc_id ~value:"allocated" ())
  |> List.of_seq

let add_debugger_variables
    ~store
    ~memory
    ~is_gil_file
    ~get_new_scope_id
    variables =
  let store_id = get_new_scope_id () in
  let memory_id = get_new_scope_id () in
  let scopes : Variable.scope list =
    [ { id = store_id; name = "Store" }; { id = memory_id; name = "Memory" } ]
  in
  let store_vars = get_store_vars store is_gil_file in
  let memory_vars = add_memory_vars memory get_new_scope_id variables in
  let vars = [ store_vars; memory_vars ] in
  let () =
    List.iter2
      (fun (scope : Variable.scope) vars ->
        Hashtbl.replace variables scope.id vars)
      scopes vars
  in
  scopes

(***** Clean-up *****)

let clean_up (keep : Expr.Set.t) (heap : t) : Expr.Set.t * Expr.Set.t =
  let forgettables =
    Hashtbl.fold
      (fun (aloc : string) (block : Block.t) forgettables ->
        match block with
        | Freed -> forgettables
        | Allocated { data; bound } -> (
            match
              (SFVL.is_empty data, bound, Expr.Set.mem (ALoc aloc) keep)
            with
            | true, None, false ->
                let () = Hashtbl.remove heap aloc in
                Expr.Set.add (Expr.ALoc aloc) forgettables
            | _ -> forgettables))
      heap Expr.Set.empty
  in
  let keep =
    Hashtbl.fold
      (fun (aloc : string) (block : Block.t) keep ->
        let keep = Expr.Set.add (ALoc aloc) keep in
        match block with
        | Freed -> keep
        | Allocated { data; _ } ->
            let data_alocs =
              Expr.Set.of_list
                (List.map
                   (fun x -> Expr.ALoc x)
                   (SS.elements (SFVL.alocs data)))
            in
            let data_lvars =
              Expr.Set.of_list
                (List.map
                   (fun x -> Expr.LVar x)
                   (SS.elements (SFVL.lvars data)))
            in
            Expr.Set.union keep (Expr.Set.union data_alocs data_lvars))
      heap keep
  in
  let forgettables = Expr.Set.diff forgettables keep in
  (forgettables, keep)
