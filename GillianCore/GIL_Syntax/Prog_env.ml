open Syntaxes.Option
module StringMap = Containers.StringMap

module Datatype_env = struct
  type t = {
    datatypes : Datatype.t StringMap.t;
    constructors : Constructor.t StringMap.t;
    cycles : SS.t StringMap.t;
  }

  type _ Effect.t += Get_datatype_env : t Effect.t

  let get () = Effect.perform Get_datatype_env

  let check_constructor_param datatype_tbl (c : Constructor.t) =
    let open Type in
    function
    | Some (DatatypeType n) ->
        if not (Hashtbl.mem datatype_tbl n) then
          let msg =
            Fmt.str "Unknown datatype %s in definition of constructor %s" n
              c.constructor_name
          in
          raise
            (Gillian_result.Exc.compilation_error ?loc:c.constructor_loc msg)
    | _ -> ()

  let check_constructor datatype_tbl cs (c : Constructor.t) =
    let () =
      if StringMap.mem c.constructor_name cs then
        let msg = "Duplicate constructor name " ^ c.constructor_name in
        raise (Gillian_result.Exc.compilation_error ?loc:c.constructor_loc msg)
    in
    List.iter (check_constructor_param datatype_tbl c) c.constructor_fields

  let find_cycles datatype_tbl =
    let get_edge =
      Hashtbl.memoize @@ fun name ->
      let (d : Datatype.t) = Hashtbl.find datatype_tbl name in
      d.datatype_constructors
      |> List.concat_map @@ fun (c : Constructor.t) ->
         c.constructor_fields
         |> List.filter_map @@ function
            | Some (Type.DatatypeType name') -> Some name'
            | _ -> None
    in
    let iter_vertices f = Hashtbl.iter (fun k _ -> f k) datatype_tbl in
    let cycle_lists = Tarjan.tarjan iter_vertices get_edge in
    List.fold_left
      (fun cycles cycle_list ->
        let current_cycle = SS.of_list cycle_list in
        SS.fold
          (fun v cycles -> StringMap.add v current_cycle cycles)
          current_cycle cycles)
      StringMap.empty cycle_lists

  (* Initialises the datatype env, ensuring datatype definitions are well formed. *)
  let make' datatype_tbl =
    let ds = ref StringMap.empty in
    let cs = ref StringMap.empty in
    let () =
      datatype_tbl
      |> Hashtbl.iter @@ fun name (d : Datatype.t) ->
         ds := StringMap.add name d !ds;
         d.datatype_constructors
         |> List.iter @@ fun c ->
            check_constructor datatype_tbl !cs c;
            cs := StringMap.add c.constructor_name c !cs
    in
    let cycles = find_cycles datatype_tbl in
    { datatypes = !ds; constructors = !cs; cycles }

  let make prog = make' prog.Prog.datatypes

  let using (t : t) f =
    match f () with
    | x -> x
    | effect Get_datatype_env, k -> Effect.Deep.continue k t

  let using_prog prog f = using (make prog) f
  let get_datatypes () = (get ()).datatypes
  let get_datatype name = StringMap.find_opt name (get_datatypes ())

  let get_datatype_exn name =
    match get_datatype name with
    | Some d -> d
    | None ->
        Fmt.failwith "Datatype_env.get_datatype_exn: datatype %s not found" name

  let get_datatype_cycle name =
    Option.value ~default:SS.empty (StringMap.find_opt name (get ()).cycles)

  let get_constructor cname = StringMap.find_opt cname (get ()).constructors

  let get_constructor_exn cname =
    match get_constructor cname with
    | Some c -> c
    | None ->
        Fmt.failwith
          "Datatype_env.get_constructor_exn: constructor %s not found" cname

  let get_constructor_type cname : Type.t option =
    let+ c = get_constructor cname in
    Type.DatatypeType c.constructor_datatype

  let get_constructor_type_exn cname : Type.t =
    match get_constructor_type cname with
    | Some t -> t
    | None ->
        Fmt.failwith
          "Datatype_env.get_constructor_type_exn: constructor %s not found."
          cname

  let get_constructor_field_types cname : Type.t option list option =
    let+ c = get_constructor cname in
    c.constructor_fields

  let get_constructor_field_types_exn cname : Type.t option list =
    match get_constructor_field_types cname with
    | Some ts -> ts
    | None ->
        Fmt.failwith
          "Datatype_env.get_constructor_field_types_exn: constructor %s not \
           found."
          cname
end

module Function_env = struct
  type t = Func.t StringMap.t
  type _ Effect.t += Get_function_env : t Effect.t

  let get () = Effect.perform Get_function_env
  let make' func_tbl = Hashtbl.fold StringMap.add func_tbl StringMap.empty
  let make prog = make' prog.Prog.funcs

  let using (t : t) f =
    match f () with
    | x -> x
    | effect Get_function_env, k -> Effect.Deep.continue k t

  let using_prog prog f = using (make prog) f
  let get_function fname = StringMap.find_opt fname (get ())

  let get_function_param_types fname =
    let+ func = get_function fname in
    List.map snd func.func_params

  let get_functions () = get ()
end

type t = Datatype_env.t * Function_env.t

let make prog = (Datatype_env.make prog, Function_env.make prog)
let using (dt, fn) f = Datatype_env.using dt (fun () -> Function_env.using fn f)

let using_prog prog f =
  Datatype_env.using_prog prog (fun () -> Function_env.using_prog prog f)
