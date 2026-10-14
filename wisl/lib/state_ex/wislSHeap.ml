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

  let is_empty ?(freed_is_empty = false) t =
    match t with
    | Freed -> freed_is_empty
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

module SMap = Gillian.Utils.Prelude.Map.Make (struct
  include String

  let of_yojson = function
    | `String s -> Ok s
    | _ -> Error "string_of_yojson: expected string"

  let to_yojson s = `String s
end)

type t = Block.t SMap.t [@@deriving yojson]

(* A symbolic heap is an immutable map from location and offset to symbolic values *)

let init () = SMap.empty

(* Simply initializes an empty heap *)

let update heap loc block =
  if Block.is_empty block then SMap.remove loc heap else SMap.add loc block heap

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
  SMap.add loc Block.Freed heap

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
  (SMap.add loc block heap, loc)

let dispose (heap : t) loc =
  match SMap.find_opt loc heap with
  | None -> error (MissingResource (Cell, loc, None))
  | Some (Allocated { data = _; bound = None }) ->
      error (MissingResource (Bound, loc, None))
  | Some Freed -> error (DoubleFree loc)
  | Some (Allocated { data; bound = Some i }) ->
      let has_all =
        let rec check j =
          j >= i
          || ((Option.is_some @@ SFVL.get (Expr.int j) data) && check (j + 1))
        in
        check 0
      in
      if has_all then ok (set_freed_with_logging heap loc)
      else error (MissingResource (Bound, loc, None))

let get_cell heap loc ofs =
  let open Delayed_result in
  match SMap.find_opt loc heap with
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

let set_cell ~alloc_if_missing heap loc ofs v =
  match SMap.find_opt loc heap with
  | None ->
      if alloc_if_missing then
        let data = SFVL.add ofs v SFVL.empty in
        let bound = None in
        ok (SMap.add loc (Block.Allocated { data; bound }) heap)
      else error (MissingResource (Cell, loc, Some ofs))
  | Some Block.Freed -> error (UseAfterFree loc)
  | Some (Allocated { data; bound }) ->
      let** () =
        match bound with
        | None -> ok ()
        | Some n ->
            let n = Expr.int n in
            let open Expr.Infix in
            if%sat n <= ofs then error (OutOfBounds (bound, loc, ofs))
            else ok ()
      in
      let* { pfs; gamma; matching } = Delayed.leak_pc_copy () in
      let equality_test = Solver.is_equal ~matching ~pfs ~gamma in
      let data = SFVL.add_with_test ~equality_test ofs v data in
      ok (SMap.add loc (Block.Allocated { data; bound }) heap)

let rem_cell heap loc offset =
  match SMap.find_opt loc heap with
  | None -> error (MissingResource (Cell, loc, Some offset))
  | Some Block.Freed -> error (UseAfterFree loc)
  | Some (Allocated { bound; data }) ->
      let data, removed = SFVL.remove offset data in
      if not removed then error (MissingResource (Cell, loc, Some offset))
      else ok (update heap loc (Allocated { bound; data }))

let get_bound heap loc =
  match SMap.find_opt loc heap with
  | Some Block.Freed -> error (UseAfterFree loc)
  | None -> error (MissingResource (Cell, loc, None))
  | Some (Allocated { bound = None; _ }) ->
      error (MissingResource (Bound, loc, None))
  | Some (Allocated { bound = Some bound; _ }) -> ok bound

let set_bound ~alloc_if_missing heap loc bound =
  let** prev =
    match (SMap.find_opt loc heap, alloc_if_missing) with
    | Some b, _ -> ok b
    | None, true -> ok Block.empty
    | None, false -> error (MissingResource (Cell, loc, None))
  in
  match prev with
  | Freed -> error (UseAfterFree loc)
  | Allocated { data; _ } ->
      let changed = Block.Allocated { data; bound = Some bound } in
      ok (SMap.add loc changed heap)

let rem_bound heap loc =
  match SMap.find_opt loc heap with
  | Some Block.Freed -> error (UseAfterFree loc)
  | None -> error (MissingResource (Cell, loc, None))
  | Some (Allocated { bound = None; _ }) ->
      error (MissingResource (Bound, loc, None))
  | Some (Allocated { bound = Some _; data }) ->
      ok (update heap loc (Allocated { data; bound = None }))

let get_freed heap loc =
  match SMap.find_opt loc heap with
  | Some Block.Freed -> ok ()
  | Some _ -> error MemoryLeak
  | None -> error (MissingResource (Freed, loc, None))

let set_freed ~alloc_if_missing heap loc =
  match (SMap.find_opt loc heap, alloc_if_missing) with
  | Some _, _ | None, true -> ok (set_freed_with_logging heap loc)
  | None, false -> error (MissingResource (Cell, loc, None))

let rem_freed heap loc =
  match SMap.find_opt loc heap with
  | Some Block.Freed -> ok (SMap.remove loc heap)
  | None -> error (MissingResource (Freed, loc, None))
  | Some _ -> error MemoryLeak

(***** Some things specific to symbolic heaps ********)

(** tries merging two locations -- returns false is the merging failed as they
    overlapped, meaning substitution must vanish! *)
let merge_loc ~new_loc ~old_loc (heap : t) : t * bool =
  let old_block, new_block =
    (SMap.find_opt old_loc heap, SMap.find_opt new_loc heap)
  in
  match (old_block, new_block) with
  | None, _ -> (heap, true)
  | Some block, None ->
      let heap = SMap.add new_loc block heap in
      let heap = SMap.remove old_loc heap in
      (heap, true)
  | Some (Allocated _ | Freed), Some Freed -> (heap, false)
  | Some Freed, Some (Allocated _) -> (heap, false)
  | ( Some (Allocated { data = data_l; bound = bound_l }),
      Some (Allocated { data = data_r; bound = bound_r }) ) ->
      let data, ok = SFVL.union data_l data_r in
      let bound, ok =
        match (bound_l, bound_r) with
        | Some _, Some _ -> (None, false)
        | None, b | b, None -> (b, ok)
      in
      let heap = SMap.add new_loc (Block.Allocated { data; bound }) heap in
      let heap = SMap.remove old_loc heap in
      (heap, ok)

let substitution subst heap : t Delayed.t =
  (* First we replace in the offset and values using fvl *)
  let heap = SMap.map (Block.substitution ~partial:true subst) heap in
  (* Then we replace within the locations themselves *)
  let aloc_subst =
    Subst.filter subst (fun var _ ->
        match var with
        | ALoc _ -> true
        | _ -> false)
  in
  let heap, subst_ok =
    Subst.fold aloc_subst
      (fun aloc new_loc (heap, acc) ->
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
        if acc then merge_loc heap ~new_loc ~old_loc else (heap, false))
      (heap, true)
  in
  if subst_ok then Delayed.return heap else Delayed.vanish ()

let assertions heap =
  SMap.fold (fun loc block acc -> Block.assertions ~loc block @ acc) heap []

let lvars heap : SS.t =
  SMap.fold (fun _ block acc -> SS.union (Block.lvars block) acc) heap SS.empty

let alocs heap : SS.t =
  SMap.fold
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
    ( Fmt.iter_bindings ~sep:(Fmt.any "@\n@\n") SMap.iter @@ fun ft (l, b) ->
      Block.pp ~loc:l ft b )
    heap

let to_seq heap =
  SMap.to_seq heap
  |> Seq.map (fun (loc, block) ->
         match block with
         | Block.Freed -> (loc, None)
         | Allocated { data; bound } -> (loc, Some (data, bound)))

let is_empty ?freed_is_empty t =
  SMap.for_all (fun _ block -> Block.is_empty ?freed_is_empty block) t
