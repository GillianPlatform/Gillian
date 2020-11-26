open Gillian.Concrete
module Literal = Gillian.Gil_syntax.Literal

type vt = Values.t

type st = Subst.t

type err_t = unit

type fix_t = unit

type t = WislCHeap.t

type action_ret = ASucc of (t * vt list) | AFail of err_t list

let init = WislCHeap.init

let copy = WislCHeap.copy

let pp fmt h = Format.fprintf fmt "%s" (WislCHeap.str h)

let pp_err fmt () = ()

let ga_to_setter = WislLActions.ga_to_setter_str

let ga_to_getter = WislLActions.ga_to_getter_str

let ga_to_deleter = WislLActions.ga_to_deleter_str

let ga_loc_indexes a_id =
  WislLActions.(
    match ga_from_str a_id with
    | Cell -> [ 0 ])

(* Small util for retrocompat *)
let vstr v = Format.asprintf "%a" Values.pp v

(* GetCell takes one argument, which supposedly evaluates to a pointer *)
let get_cell heap params =
  Literal.(
    match params with
    | [ Loc loc; Int offset ] -> (
        match WislCHeap.get heap loc offset with
        | Some value -> ASucc (heap, [ Loc loc; Int offset; value ])
        | None       -> AFail [])
    | l                       ->
        failwith
          (Printf.sprintf
             "Invalid parameters for Wisl GetCell Local Action : [ %s ] "
             (String.concat ", " (List.map vstr l))))

let set_cell heap params =
  Literal.(
    match params with
    | [ Loc loc; Int offset; value ] ->
        let () = WislCHeap.set heap loc offset value in
        ASucc (heap, [])
    | l ->
        failwith
          (Printf.sprintf
             "Invalid parameters for Wisl SetCell Local Action : [ %s ] "
             (String.concat ", " (List.map vstr l))))

let rem_cell heap params =
  Literal.(
    match params with
    | [ Loc loc; Int offset ] ->
        let () = WislCHeap.remove heap loc offset in
        ASucc (heap, [])
    | l                       ->
        failwith
          (Printf.sprintf
             "Invalid parameters for Wisl SetCell Local Action : [ %s ] "
             (String.concat ", " (List.map vstr l))))

let alloc heap params =
  Literal.(
    match params with
    | [ Int size ] when size >= 1 ->
        let loc = WislCHeap.alloc heap size in
        let litloc = Loc loc in
        ASucc (heap, [ litloc; Int 0 ])
        (* returns a pointer to the first element *)
    | l ->
        failwith
          (Printf.sprintf
             "Invalid parameters for Wisl Alloc Local Action : [ %s ] "
             (String.concat ", " (List.map vstr l))))

let dispose heap params =
  let open Literal in
  match params with
  | [ Loc obj ] ->
      let () = WislCHeap.dispose heap obj in
      ASucc (heap, [])
  | l           ->
      failwith
        (Printf.sprintf
           "Invalid parameters for Wisl Dispose Local Action : [ %s ] "
           (String.concat ", " (List.map vstr l)))

let execute_action name heap params =
  let action = WislLActions.ac_from_str name in
  WislLActions.(
    match action with
    | GetCell -> get_cell heap params
    | SetCell -> set_cell heap params
    | RemCell -> rem_cell heap params
    | Alloc   -> alloc heap params
    | Dispose -> dispose heap params)

(** Non-implemented functions *)
let assertions ?to_keep:_ _ =
  raise (Failure "ERROR: to_assertions called for concrete executions")

let lvars _ = raise (Failure "ERROR: get_lvars called for concrete executions")

let clean_up _ = raise (Failure "Cleanup of concrete state.")

let fresh_val _ = raise (Failure "fresh_val not implemented in concrete state")

let substitution_in_place _ _ =
  raise (Failure "substitution_in_place not implemented in concrete state")

let is_overlapping_asrt _ = false
