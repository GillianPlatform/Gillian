open Gillian.Concrete
open Gillian.Gil_syntax
open Javert_utils

module M : Memory_S with type init_data = unit = struct
  type init_data = unit
  type vt = Values.t

  (** Type of JSIL general states *)
  type t = CHeap.t

  (** Type of JSIL substitutions *)
  type st = Subst.t

  (** Errors *)
  type err_t = unit [@@deriving yojson, show]

  type action_ret = (t * vt list, err_t) result

  let pp = CHeap.pp
  let copy = CHeap.copy
  let init = CHeap.init

  let set_cell (heap : t) (loc : vt) (prop : vt) (v : vt option) : action_ret =
    let loc, prop =
      match (loc, prop) with
      | Loc loc, String prop -> (loc, prop)
      | _ -> raise (Failure "C Heap Update: illegal heap update")
    in

    let obj = CHeap.get heap loc in
    match obj with
    | None -> raise (Failure "C Heap Update: object not found")
    | Some (obj, _) ->
        (match v with
        | None -> CObject.remove obj prop
        | Some v when Values.to_literal v = Some Nono -> CObject.remove obj prop
        | Some v -> CObject.set obj prop v);
        Ok (heap, [])

  let get_cell ?(remove : bool option) (heap : t) (loc : vt) (prop : vt) :
      action_ret =
    let loc, prop =
      match (loc, prop) with
      | Loc loc, String prop -> (loc, prop)
      | _ -> raise (Failure "Illegal get_cell")
    in

    let remove = Option.value ~default:false remove in
    if remove then
      raise (Failure "Concrete get_cell. Remove Option must be implemented!")
    else
      match CHeap.get heap loc with
      | None -> Error ()
      | Some (obj, _) ->
          let v = Option.value ~default:Literal.Nono (CObject.get obj prop) in
          Ok (heap, [ Loc loc; String prop; v ])

  let get_domain ?expected_props:_ ?(remove : bool option) (heap : t) (loc : vt)
      : action_ret =
    let loc =
      match loc with
      | Loc loc -> loc
      | _ -> raise (Failure "Illegal get_domain")
    in

    let remove = Option.value ~default:false remove in
    if remove then
      raise (Failure "Concrete get_domain. Remove Option must be implemented!")
    else
      match CHeap.get heap loc with
      | None -> Error ()
      | Some (obj, _) ->
          let props = CObject.properties obj in
          Ok
            ( heap,
              [
                Loc loc;
                LList (List.map (fun prop -> Literal.String prop) props);
              ] )

  let get_metadata ?(remove = false) (heap : t) (loc : vt) : action_ret =
    let loc =
      match loc with
      | Loc loc -> loc
      | _ -> raise (Failure "Illegal get_metadata")
    in

    if remove then
      raise
        (Failure "Concrete get_metadata. Remove Option must be implemented!")
    else
      match CHeap.get heap loc with
      | None -> Error ()
      | Some (_, vm) -> Ok (heap, [ Loc loc; vm ])

  let delete_object (heap : t) (loc : vt) : action_ret =
    let loc =
      match loc with
      | Loc loc -> loc
      | _ -> raise (Failure "Illegal get_domain")
    in
    match CHeap.get heap loc with
    | None -> raise (Failure "delete_obj. location does NOT exist in the heap")
    | Some _ ->
        CHeap.remove heap loc;
        Ok (heap, [])

  let alloc (heap : t) (loc : vt option) (mv : vt) : action_ret =
    let new_loc =
      match loc with
      | None -> Generators.fresh_loc ()
      | Some (Loc loc) -> loc
      | _ -> raise (Failure "C Allocation: non-loc loc argument")
    in
    CHeap.set heap new_loc (CObject.init (), mv);
    let new_loc_lit : Literal.t = Loc new_loc in
    Ok (heap, [ new_loc_lit ])

  (** Execute action *)
  let execute_action (action : string) (heap : t) (args : vt list) : action_ret
      =
    if action = JSILNames.getCell then
      match args with
      | [ loc; prop ] -> get_cell heap loc prop
      | _ -> raise (Failure "Internal Error. execute_action")
    else if action = JSILNames.setCell then
      match args with
      | [ loc; prop; v ] -> set_cell heap loc prop (Some v)
      | _ -> raise (Failure "Internal Error. execute_action")
    else if action = JSILNames.delCell then
      match args with
      | [ loc; prop ] -> set_cell heap loc prop None
      | _ -> raise (Failure "Internal Error. execute_action")
    else if action = JSILNames.alloc then
      match args with
      | [ Empty; m_loc ] -> alloc heap None m_loc
      | [ loc; m_loc ] -> alloc heap (Some loc) m_loc
      | _ -> raise (Failure "Internal Error. execute_action")
    else if action = JSILNames.delObj then
      match args with
      | [ loc ] -> delete_object heap loc
      | _ -> raise (Failure "Internal Error. execute_action")
    else if action = JSILNames.getAllProps then
      match args with
      | [ loc ] -> get_domain heap loc
      | _ -> raise (Failure "Internal Error. execute_action")
    else if action = JSILNames.getMetadata then
      match args with
      | [ loc ] -> get_metadata heap loc
      | _ -> raise (Failure "Internal Error. execute_action")
    else raise (Failure "Internal Error. execute_action")

  let pp_err _ _ = ()
end
