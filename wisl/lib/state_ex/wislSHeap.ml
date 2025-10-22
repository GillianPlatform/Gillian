(***** This module defines a Wisl Symbolic Heap *******)
open Gillian.Symbolic
open Gillian.Monadic
open Delayed_result
open Delayed.Syntax
open Delayed_result.Syntax
open Gillian.Gil_syntax
module Solver = Gillian.Logic.FOSolver
module Reduction = Gillian.Logic.Reduction

type err =
  | MissingResource of (WislLActions.ga * string * Expr.t option)
  | DoubleFree of string
  | UseAfterFree of string
  | MemoryLeak
  | OutOfBounds of (int option * string * Expr.t)
  | InvalidLocation of Expr.t
[@@deriving yojson, show]

module Block = struct
  type t = Freed | Allocated of { data : SFVL.t; bound : int option }
  [@@deriving yojson]

  let empty = Allocated { data = SFVL.empty; bound = None }

  let is_empty t =
    match t with
    | Freed -> false
    | Allocated { data; bound } -> SFVL.is_empty data && Option.is_none bound

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
            ~constr:(fun loc offset value -> Constr.cell ~loc ~offset ~value ())
            eloc data
        in
        let bound_asrt =
          match bound with
          | None -> []
          | Some bound -> [ Constr.bound ~loc:eloc ~bound () ]
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

let update heap loc block =
  if Block.is_empty block then Hashtbl.remove heap loc
  else Hashtbl.replace heap loc block

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
      Logging.Logging_constants.Content_type.set_freed_info
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
  | None -> error (MissingResource (Cell, loc, None))
  | Some (Allocated { data = _; bound = None }) ->
      error (MissingResource (Bound, loc, None))
  | Some Freed -> error (DoubleFree loc)
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
        ok ()
      else error (MissingResource (Bound, loc, None))

let get_cell heap loc ofs =
  let open Delayed_result in
  match Hashtbl.find_opt heap loc with
  | None -> error (MissingResource (Cell, loc, Some ofs))
  | Some Block.Freed -> error (UseAfterFree loc)
  | Some (Allocated { data; bound }) -> (
      let maybe_out_of_bound =
        match bound with
        | None -> Expr.false_
        | Some n -> Expr.Infix.(Expr.int n <= ofs)
      in
      if%sat maybe_out_of_bound then error (OutOfBounds (bound, loc, ofs))
      else
        match SFVL.get ofs data with
        | Some v -> ok (loc, ofs, v)
        | None -> (
            let* { pfs; gamma; matching } = Delayed.leak_pc_copy () in
            match
              SFVL.get_first
                (fun name -> Solver.is_equal ~pfs ~gamma ~matching name ofs)
                data
            with
            | Some (o, v) -> ok (loc, o, v)
            | None -> error (MissingResource (Cell, loc, Some ofs))))

let set_cell heap loc_name ofs v =
  match Hashtbl.find_opt heap loc_name with
  | None ->
      let data = SFVL.add ofs v SFVL.empty in
      let bound = None in
      let () = Hashtbl.replace heap loc_name (Allocated { data; bound }) in
      ok ()
  | Some Block.Freed -> error (UseAfterFree loc_name)
  | Some (Allocated { data; bound }) ->
      let** () =
        match bound with
        | None -> ok ()
        | Some n ->
            let n = Expr.int n in
            let open Expr.Infix in
            if%sat n <= ofs then error (UseAfterFree loc_name) else ok ()
      in
      let* { pfs; gamma; matching } = Delayed.leak_pc_copy () in
      let equality_test = Solver.is_equal ~matching ~pfs ~gamma in
      let data = SFVL.add_with_test ~equality_test ofs v data in
      let () = Hashtbl.replace heap loc_name (Allocated { data; bound }) in
      ok ()

let rem_cell heap loc offset =
  match Hashtbl.find_opt heap loc with
  | None -> error (MissingResource (Cell, loc, Some offset))
  | Some Block.Freed -> error (UseAfterFree loc)
  | Some (Allocated { bound; data }) ->
      let data, removed = SFVL.remove offset data in
      if not removed then error (MissingResource (Cell, loc, Some offset))
      else
        let () = update heap loc (Allocated { bound; data }) in
        ok ()

let get_bound heap loc =
  match Hashtbl.find_opt heap loc with
  | Some Block.Freed -> error (UseAfterFree loc)
  | None -> error (MissingResource (Cell, loc, None))
  | Some (Allocated { bound = None; _ }) ->
      error (MissingResource (Bound, loc, None))
  | Some (Allocated { bound = Some bound; _ }) -> ok bound

let set_bound heap loc bound =
  let prev = Option.value ~default:Block.empty (Hashtbl.find_opt heap loc) in
  match prev with
  | Freed -> error (UseAfterFree loc)
  | Allocated { data; _ } ->
      let changed = Block.Allocated { data; bound = Some bound } in
      let () = Hashtbl.replace heap loc changed in
      ok ()

let rem_bound heap loc =
  match Hashtbl.find_opt heap loc with
  | Some Block.Freed -> error (UseAfterFree loc)
  | None -> error (MissingResource (Cell, loc, None))
  | Some (Allocated { bound = None; _ }) ->
      error (MissingResource (Bound, loc, None))
  | Some (Allocated { bound = Some _; data }) ->
      let () = update heap loc (Allocated { data; bound = None }) in
      ok ()

let get_freed heap loc =
  match Hashtbl.find_opt heap loc with
  | Some Block.Freed -> ok ()
  | Some _ -> error MemoryLeak
  | None -> error (MissingResource (Freed, loc, None))

let set_freed heap loc =
  set_freed_with_logging heap loc;
  Delayed.return ()

let rem_freed heap loc =
  match Hashtbl.find_opt heap loc with
  | Some Block.Freed ->
      Hashtbl.remove heap loc;
      ok ()
  | None -> error (MissingResource (Freed, loc, None))
  | Some _ -> error MemoryLeak

(***** Some things specific to symbolic heaps ********)

(** tries merging two locations -- returns false is the merging failed as they
    overlapped, meaning substitution must vanish! *)
let merge_loc ~new_loc ~old_loc (heap : t) : bool =
  let old_block, new_block =
    (Hashtbl.find_opt heap old_loc, Hashtbl.find_opt heap new_loc)
  in
  match (old_block, new_block) with
  | None, _ -> true
  | Some block, None ->
      Hashtbl.replace heap new_loc block;
      Hashtbl.remove heap old_loc;
      true
  | Some (Allocated _ | Freed), Some Freed -> false
  | Some Freed, Some (Allocated _) -> false
  | ( Some (Allocated { data = data_l; bound = bound_l }),
      Some (Allocated { data = data_r; bound = bound_r }) ) ->
      let data, ok = SFVL.union data_l data_r in
      let bound, ok =
        match (bound_l, bound_r) with
        | Some _, Some _ -> (None, false)
        | None, b | b, None -> (b, ok)
      in
      let () = Hashtbl.replace heap new_loc (Allocated { data; bound }) in
      Hashtbl.remove heap old_loc;
      ok

let substitution_in_place subst heap : t Delayed.t =
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
  let subst_ok =
    Subst.fold aloc_subst
      (fun aloc new_loc acc ->
        let old_loc =
          match aloc with
          | ALoc loc -> loc
          | _ -> raise (Failure "Impossible by construction")
        in
        let new_loc =
          match new_loc with
          | Expr.Lit (Literal.Loc loc) -> loc
          | Expr.ALoc loc -> loc
          | _ ->
              raise
                (Failure
                   (Printf.sprintf "Heap substitution fail for loc: %s"
                      ((WPrettyUtils.to_str Expr.pp) new_loc)))
        in
        acc && merge_loc heap ~new_loc ~old_loc)
      true
  in
  if subst_ok then Delayed.return heap else Delayed.vanish ()

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

let to_seq heap =
  Hashtbl.to_seq heap
  |> Seq.map (fun (loc, block) ->
         match block with
         | Block.Freed -> (loc, None)
         | Allocated { data; bound } -> (loc, Some (data, bound)))

let is_empty t = Hashtbl.to_seq_values t |> Seq.for_all Block.is_empty

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
