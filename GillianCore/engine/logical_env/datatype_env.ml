type constructors_tbl = (string, Constructor.t) Hashtbl.t
type datatypes_tbl = (string, Datatype.t) Hashtbl.t
type t = { constructors : constructors_tbl; datatypes : datatypes_tbl }

let datatype_env : t option ref = ref None

(* Initialises the datatype env, ensuring datatype definitions are well formed. *)
let init datatypes =
  let constructors = Hashtbl.create Config.medium_tbl_size in

  let check_type topt =
    let open Type in
    match topt with
    | Some (DatatypeType n) ->
        if not (Hashtbl.mem datatypes n) then
          let msg = "Unknown type in constructor definition: " ^ n in
          Logging.fail msg
    | _ -> ()
  in

  let add_constructor_to_tbl (c : Constructor.t) =
    if Hashtbl.mem constructors c.constructor_name then
      let msg =
        "Cannot reuse datatype constructor names: " ^ c.constructor_name
      in
      Logging.fail msg
    else List.iter check_type c.constructor_fields;
    Hashtbl.add constructors c.constructor_name c
  in

  let add_constructors_to_tbl (cs : Constructor.t list) =
    List.iter add_constructor_to_tbl cs
  in

  let () =
    Hashtbl.iter
      (fun _ (d : Datatype.t) ->
        add_constructors_to_tbl d.datatype_constructors)
      datatypes
  in

  datatype_env := Some { constructors; datatypes }

let is_initialised () =
  match !datatype_env with
  | None -> false
  | Some _ -> true

let get_constructor_type cname : Type.t option =
  let delta = !datatype_env in
  let constructor =
    Option.map (fun delta -> Hashtbl.find_opt delta.constructors cname) delta
  in
  Option.map
    (fun (c : Constructor.t) -> Type.DatatypeType c.constructor_datatype)
    (Option.join constructor)

let get_constructor_type_unsafe cname : Type.t =
  let typ = get_constructor_type cname in
  match typ with
  | Some t -> t
  | None ->
      raise
        (Failure
           ("Datatype_env.get_constructor_type_unsafe: constructor " ^ cname
          ^ " not found."))

let get_constructor_field_types cname : Type.t option list option =
  let delta = !datatype_env in
  let constructor =
    Option.map (fun delta -> Hashtbl.find_opt delta.constructors cname) delta
  in
  Option.map
    (fun (c : Constructor.t) -> c.constructor_fields)
    (Option.join constructor)

let get_constructor_field_types_unsafe cname : Type.t option list =
  let ts = get_constructor_field_types cname in
  match ts with
  | Some ts -> ts
  | None ->
      raise
        (Failure
           ("Datatype_env.get_constructor_field_types_unsafe: constructor "
          ^ cname ^ " not found."))

let get_datatypes () : Datatype.t list =
  let res =
    Option.map
      (fun delta -> List.of_seq (Hashtbl.to_seq_values delta.datatypes))
      !datatype_env
  in
  Option.value ~default:[] res
