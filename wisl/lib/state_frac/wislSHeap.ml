(***** This module defines a Wisl Symbolic Heap *******)
open Gillian.Symbolic
open Gillian.Gil_syntax
open Gillian.Monadic
open Delayed.Syntax
open Delayed_result.Syntax
open Delayed_result
module Solver = Gillian.Logic.FOSolver
module Reduction = Gillian.Logic.Reduction
open Gillian.Debugger.Utils
module PFS = Gillian.Symbolic.Pure_context
module L = Logging

type err =
  | MissingResource of (WislLActions.ga * string * Expr.t option)
  | DoubleFree of string
  | UseAfterFree of string
  | MemoryLeak
  | OutOfBounds of (int option * string * Expr.t)
  | InvalidLocation of Expr.t
[@@deriving yojson, show]

module Block = struct
  type t =
    | Freed
    | Allocated of { data : SFVL.t; bound : (int * Expr.t) option }
  [@@deriving yojson]

  let empty = Allocated { data = SFVL.empty; bound = None }

  let is_empty = function
    | Freed -> false
    | Allocated { data; bound } -> SFVL.is_empty data && Option.is_none bound

  let substitution ~partial subst block =
    match block with
    | Freed -> Freed
    | Allocated { data; bound } ->
        let data = SFVL.substitution subst partial data in
        let bound =
          Option.map
            (fun (b, p) -> (b, Subst.subst_in_expr subst ~partial p))
            bound
        in
        Allocated { data; bound }

  let assertions ~loc block =
    let eloc = Expr.loc_from_loc_name loc in
    match block with
    | Freed -> [ Constr.freed ~loc:eloc ]
    | Allocated { data; bound } ->
        let data_asrts =
          SFVL.assertions_with_constructor
            ~constr:(fun loc offset SFVL.{ value; permission } ->
              Constr.cell ~loc ~offset ~value ~permission ())
            eloc data
        in
        let bound_asrt =
          match bound with
          | None -> []
          | Some (bound, permission) ->
              [ Constr.bound ~loc:eloc ~bound ~permission () ]
        in
        bound_asrt @ data_asrts

  let pp ~loc fmt block =
    match block with
    | Freed -> Fmt.pf fmt "%s -> FREED" loc
    | Allocated { data; bound } ->
        Fmt.pf fmt "%s -> @[<v>BOUND: %a@ %a@]" loc
          (Fmt.Dump.option (Fmt.Dump.pair Fmt.int Expr.pp))
          bound
          (Fmt.braces @@ Fmt.vbox
          @@ Fmt.iter_bindings ~sep:Fmt.sp SFVL.iter
          @@ fun ft (o, SFVL.{ value; permission }) ->
          Fmt.pf ft "%a: %a [%a]" Expr.pp o Expr.pp value Expr.pp permission)
          data

  let lvars block =
    match block with
    | Freed -> SS.empty
    | Allocated { data; bound } ->
        SS.union (SFVL.lvars data)
          (Option.fold ~none:SS.empty ~some:(fun (_, p) -> Expr.lvars p) bound)

  let alocs block =
    match block with
    | Freed -> SS.empty
    | Allocated { data; bound } ->
        SS.union (SFVL.alocs data)
          (Option.fold ~none:SS.empty ~some:(fun (_, p) -> Expr.alocs p) bound)
end

type t = (string, Block.t) Hashtbl.t [@@deriving yojson]

(* A symbolic heap is a map from location and offset to symbolic values *)

let init () = Hashtbl.create 1

(* Simply initializes an empty heap *)

(****** Standard stuff about hashtbls ********)

let copy heap = Hashtbl.copy heap

(***** Implementation of local actions *****)

let alloc (heap : t) size =
  let loc = ALoc.alloc () in
  let rec get_list current_offset =
    if current_offset < 0 then []
    else
      ( Expr.int current_offset,
        SFVL.{ value = Expr.Lit Literal.Null; permission = Lit (Num 1.0) } )
      :: get_list (current_offset - 1)
  in
  let l = get_list (size - 1) in
  let sfvl = SFVL.of_list l in
  let block =
    Block.Allocated { data = sfvl; bound = Some (size, Lit (Num 1.0)) }
  in
  let () = Hashtbl.replace heap loc block in
  loc

let dispose (heap : t) loc =
  match Hashtbl.find_opt heap loc with
  | None -> error (MissingResource (Cell, loc, None))
  | Some Freed -> error (DoubleFree loc)
  | Some (Allocated { data = _; bound = None; _ }) ->
      error (MissingResource (Bound, loc, None))
  | Some (Allocated { data; bound = Some (i, perm) }) ->
      let full_perm = Expr.num 1.0 in
      if%sat Expr.Infix.(perm <. full_perm) then
        error (MissingResource (Bound, loc, None))
      else
        let rec aux = function
          | 0 -> ok ()
          | n -> (
              match SFVL.get (Expr.int i) data with
              | None -> error (MissingResource (Bound, loc, None))
              | Some { permission = q; _ } ->
                  if%sat Expr.Infix.(q <. full_perm) then
                    let missing_permission = Expr.Infix.(full_perm -. q) in
                    error (MissingResource (Cell, loc, Some missing_permission))
                  else aux (n - 1))
        in
        let++ () = aux i in
        Hashtbl.replace heap loc Block.Freed

(* Helper function: Checks if the offset exists in the SFVL first by performing a performance efficient check
   and then using the solver. If an entry is found, it applies the success_case function, otherwise the none_case *)
let check_sfvl ofs data none_case success_case =
  match SFVL.get ofs data with
  | Some SFVL.{ value; permission } -> success_case ofs value permission
  | None -> (
      let* { pfs; gamma; matching } = Delayed.leak_pc_copy () in
      match
        SFVL.get_first
          (fun name -> Solver.is_equal ~matching ~pfs ~gamma name ofs)
          data
      with
      | None -> none_case ()
      | Some (o, SFVL.{ value; permission }) -> success_case o value permission)

(* Helper function: Performs the corresponding correctness checks for a cell assertion
      with an optional check for permission accounting, which is useful in the case of producers,
      but not for regular load operations.
*)
let access_cell heap loc ofs permission_check =
  match Hashtbl.find_opt heap loc with
  | None -> error (MissingResource (Cell, loc, Some ofs))
  | Some Block.Freed -> error (UseAfterFree loc)
  | Some (Allocated { data; bound }) ->
      let** () =
        match bound with
        | None -> ok ()
        | Some (n, _) ->
            let expr_n = Expr.int n in
            if%sat Expr.Infix.(expr_n <= ofs) then
              error (OutOfBounds (Some n, loc, ofs))
            else ok ()
      in
      let none_case () = error (MissingResource (Cell, loc, Some ofs)) in
      let success_case ofs value permission =
        let* permission_check = permission_check permission in
        match permission_check with
        | Some missing_permission ->
            error (MissingResource (Cell, loc, Some missing_permission))
        | None -> ok (loc, ofs, value)
      in
      check_sfvl ofs data none_case success_case

let load heap loc ofs =
  let++ loc, ofs, v = access_cell heap loc ofs (fun _ -> Delayed.return None) in
  (Expr.loc_from_loc_name loc, ofs, v)

let get_cell heap loc ofs out_perm =
  let permission_check q =
    if%sat Expr.Infix.(q <. out_perm) then
      Delayed.return (Some Expr.Infix.(out_perm -. q))
    else Delayed.return None
  in
  access_cell heap loc ofs permission_check

(* Helper function: Performs the preliminary checks common to the set_cell and store
   operations and applies the "block_missing" operation if the block cannot be found
   in the heap "in_bounds" operation if the access to the allocated cell is within bounds *)
let overwrite_cell heap loc_name ofs block_missing in_bounds =
  match Hashtbl.find_opt heap loc_name with
  | None -> block_missing ()
  | Some Block.Freed -> error (UseAfterFree loc_name)
  | Some (Allocated { data; bound = None }) -> in_bounds data None
  | Some (Allocated { data; bound = Some (bound, perm) }) ->
      if%sat Expr.Infix.(ofs >= Expr.int bound) then
        error (MissingResource (Bound, loc_name, Some ofs))
      else in_bounds data (Some (bound, perm))

(* Helper function: Extends the block data with a new cell at offset ofs with the value v. *)
let extend_block heap loc_name ofs value data bound permission =
  let* { pfs; gamma; matching } = Delayed.leak_pc_copy () in
  let equality_test = Solver.is_equal ~matching ~pfs ~gamma in
  let data =
    SFVL.add_with_test ~equality_test ofs SFVL.{ value; permission } data
  in
  let () = Hashtbl.replace heap loc_name (Block.Allocated { data; bound }) in
  let fl = Expr.Infix.(permission >. Expr.num 0.0) in
  ok ~learned:[ fl ] ()

let store heap loc_name ofs v =
  let block_missing () = error (MissingResource (Cell, loc_name, Some ofs)) in
  let in_bounds (data : SFVL.t) bound =
    let full_perm = Expr.num 1.0 in
    let none_case () = error (MissingResource (Cell, loc_name, Some ofs)) in
    let some_case ofs _ permission =
      if%sat Expr.Infix.(permission <. full_perm) then
        let missing_permission = Expr.Infix.(full_perm -. permission) in
        error (MissingResource (Cell, loc_name, Some missing_permission))
      else
        let++ () = extend_block heap loc_name ofs v data bound permission in
        (Expr.loc_from_loc_name loc_name, ofs)
    in
    check_sfvl ofs data none_case some_case
  in
  overwrite_cell heap loc_name ofs block_missing in_bounds

let set_cell heap loc_name ofs v out_perm =
  let block_missing () =
    let data =
      SFVL.add ofs SFVL.{ value = v; permission = out_perm } SFVL.empty
    in
    let bound = None in
    let () = Hashtbl.replace heap loc_name (Block.Allocated { data; bound }) in
    ok ()
  in
  let in_bounds data bound =
    let none_case () = extend_block heap loc_name ofs v data bound out_perm in
    let some_case ofs value permission =
      let new_perm = Expr.Infix.(permission +. out_perm) in
      let data = SFVL.add ofs SFVL.{ value = v; permission = new_perm } data in
      Hashtbl.replace heap loc_name (Block.Allocated { data; bound });
      ok ~learned:[ Expr.Infix.(value == v && new_perm <=. Expr.num 1.0) ] ()
    in
    check_sfvl ofs data none_case some_case
  in
  overwrite_cell heap loc_name ofs block_missing in_bounds

let rem_cell heap loc offset out_perm =
  match Hashtbl.find_opt heap loc with
  | None -> error (MissingResource (Cell, loc, Some offset))
  | Some Block.Freed -> error (UseAfterFree loc)
  | Some (Allocated { data; bound }) -> (
      match SFVL.get offset data with
      | None -> error (MissingResource (Cell, loc, Some offset))
      | Some SFVL.{ value; permission } ->
          let data = SFVL.remove offset data in
          let new_perm = Expr.Infix.(permission -. out_perm) in
          let* data =
            if%sat Expr.Infix.(new_perm == Expr.num 0.0) then
              Delayed.return data
            else
              Delayed.return
              @@ SFVL.add offset SFVL.{ value; permission = new_perm } data
          in
          let () = Hashtbl.replace heap loc (Allocated { data; bound }) in
          ok ())

let get_bound heap loc out_perm =
  match Hashtbl.find_opt heap loc with
  | Some Block.Freed -> error (UseAfterFree loc)
  | None -> error (MissingResource (Cell, loc, None))
  | Some (Allocated { bound = None; _ }) ->
      error (MissingResource (Bound, loc, None))
  | Some (Allocated { bound = Some ((_, q) as bound); _ }) ->
      if%sat Expr.Infix.(q <. out_perm) then
        let missing_permission = Expr.Infix.(out_perm -. q) in
        error (MissingResource (Bound, loc, Some missing_permission))
      else ok bound

let set_bound heap loc b out_perm =
  let prev = Option.value ~default:Block.empty (Hashtbl.find_opt heap loc) in
  match prev with
  | Freed -> error (UseAfterFree loc)
  | Allocated { data; bound = None } ->
      let () =
        Hashtbl.replace heap loc
          (Block.Allocated { data; bound = Some (b, out_perm) })
      in
      ok ()
  | Allocated { data; bound = Some (_, permission) } ->
      let full_perm = Expr.num 1.0 in
      let new_perm = Expr.Infix.(permission +. out_perm) in
      let fl = Expr.Infix.(new_perm <=. full_perm) in
      let () =
        Hashtbl.replace heap loc
          (Block.Allocated { data; bound = Some (b, new_perm) })
      in
      ok ~learned:[ fl ] ()

let rem_bound heap loc out_perm =
  match Hashtbl.find_opt heap loc with
  | Some Block.Freed -> error (UseAfterFree loc)
  | None -> error (MissingResource (Cell, loc, None))
  | Some (Allocated { bound = None; _ }) ->
      error (MissingResource (Bound, loc, None))
  | Some (Allocated { data; bound = Some (n, q) }) ->
      let new_perm = Expr.Infix.(q -. out_perm) in
      let* bound =
        if%sat Expr.Infix.(new_perm == Expr.num 0.0) then Delayed.return None
        else Delayed.return (Some (n, new_perm))
      in
      let () = Hashtbl.replace heap loc (Allocated { data; bound }) in
      ok ()

let get_freed heap loc =
  match Hashtbl.find_opt heap loc with
  | Some Block.Freed -> ok ()
  | Some _ -> error MemoryLeak
  | None -> error (MissingResource (Freed, loc, None))

let set_freed heap loc =
  Hashtbl.replace heap loc Block.Freed;
  Delayed.return ()

let rem_freed heap loc =
  match Hashtbl.find_opt heap loc with
  | Some Block.Freed ->
      Hashtbl.remove heap loc;
      ok ()
  | None -> error (MissingResource (Freed, loc, None))
  | Some _ -> error MemoryLeak

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

let substitution_in_place subst heap =
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
        | _ -> failwith "Impossible by construction"
      in
      let new_loc_str =
        match new_loc with
        | Expr.Lit (Loc loc) -> loc
        | Expr.ALoc loc -> loc
        | _ -> Fmt.failwith "Heap substitution fail for loc: %a" Expr.pp new_loc
      in
      merge_loc heap new_loc_str aloc);
  Delayed.return heap

let is_empty t = Hashtbl.to_seq_values t |> Seq.for_all Block.is_empty

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

let to_seq (heap : t) =
  Hashtbl.to_seq heap
  |> Seq.map (fun (loc, block) ->
         match block with
         | Block.Freed -> (loc, None)
         | Allocated { data; bound } ->
             let bound = Option.map fst bound in
             (loc, Some (data, bound)))

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
    |> List.map (fun (offset, SFVL.{ value; _ }) : Variable.t ->
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
               | Some (n, _) -> string_of_int n
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
        | Allocated { data; bound; _ } -> (
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
