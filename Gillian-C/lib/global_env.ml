type err_t = Symbol_not_found of string
type def = FunDef of string | GlobVar of string

let location_of_symbol str = Gillian.Utils.Names.lloc_prefix ^ str

(** maps location to definition *)
type t = def String_map.t

let empty = String_map.empty
let add_fundef genv loc fdef = String_map.add loc (FunDef fdef) genv
let add_globvar genv loc gvar = String_map.add loc (GlobVar gvar) genv
let add_def genv loc def = String_map.add loc def genv

let of_definition_list defs =
  List.fold_left
    (fun genv (symb, def) ->
      let loc = location_of_symbol symb in
      add_def genv loc def)
    empty defs

let find_def genv loc = String_map.find loc genv

let find_def_opt genv loc =
  try Some (find_def genv loc) with Not_found -> None

let find_function_opt genv loc =
  match find_def_opt genv loc with
  | Some (FunDef f) -> Some f
  | Some (GlobVar _) ->
      failwith "Gillian-C.Global_env.find_function: Not a function!"
  | None -> None

let serialize_def def =
  let open Gil_syntax in
  let lit =
    match def with
    | FunDef fname -> Literal.LList [ String "function"; String fname ]
    | GlobVar vname -> Literal.LList [ String "variable"; String vname ]
  in
  lit

module Serialization = struct
  module Loc = struct
    open Gillian.Utils

    type t = string

    let of_yojson yjs =
      match yjs with
      | `String str when Names.is_lloc_name str -> Ok str
      | _ -> Error ("invalid symbol location: " ^ Yojson.Safe.to_string yjs)

    let to_yojson loc = `String loc
  end

  type kind = Function [@name "fun"] | Variable [@name "var"]

  let kind_to_yojson kind =
    match kind with
    | Function -> `String "fun"
    | Variable -> `String "var"

  let kind_of_yojson yjs =
    match yjs with
    | `String "fun" -> Ok Function
    | `String "var" -> Ok Variable
    | _ -> Error ("invalid symbol kind: " ^ Yojson.Safe.to_string yjs)

  type entry = { loc : Loc.t; kind : kind; name : string } [@@deriving yojson]

  let add_entry genv entry =
    match entry.kind with
    | Function -> add_fundef genv entry.loc entry.name
    | Variable -> add_globvar genv entry.loc entry.name

  let of_definition_list (entries : entry list) =
    List.fold_left add_entry empty entries

  let to_definition_list genv =
    String_map.fold
      (fun loc def acc ->
        let entry =
          match def with
          | FunDef fname -> { loc; kind = Function; name = fname }
          | GlobVar vname -> { loc; kind = Variable; name = vname }
        in
        entry :: acc)
      genv []
end

let of_yojson json =
  let json =
    match json with
    | `Null -> `List []
    | _ -> json
  in
  Result.map Serialization.of_definition_list
    ([%of_yojson: Serialization.entry list] json)

let to_yojson genv =
  let open Serialization in
  to_definition_list genv |> [%to_yojson: Serialization.entry list]

let pp ft genv =
  let open Fmt in
  let pp_binding ft (loc, def) =
    let pp_def ft = function
      | FunDef fdef -> pf ft "%s (Function)" fdef
      | GlobVar gvar -> pf ft "%s (Variable)" gvar
    in
    pf ft "%s -> %a" loc pp_def def
  in
  (Fmt.iter_bindings ~sep:(any "@\n") String_map.iter pp_binding) ft genv
