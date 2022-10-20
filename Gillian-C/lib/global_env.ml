type err_t = Symbol_not_found of string
type def = FunDef of string | GlobVar of string

let location_of_symbol str = "$l_" ^ str

type t = {
  symb : (string, string) PMap.t;  (** maps symbols to loc names *)
  defs : (string, def) PMap.t;  (** maps loc names to definitions *)
}

let empty = { symb = PMap.empty; defs = PMap.empty }

let add_fundef genv symb loc fdef =
  let symb = PMap.add symb loc genv.symb in
  let defs = PMap.add loc (FunDef fdef) genv.defs in
  { symb; defs }

let add_globvar genv symb loc gvar =
  let symb = PMap.add symb loc genv.symb in
  let defs = PMap.add loc (GlobVar gvar) genv.defs in
  { symb; defs }

let add_def genv symb loc def =
  let symb = PMap.add symb loc genv.symb in
  let defs = PMap.add loc def genv.defs in
  { symb; defs }

let of_definition_list defs =
  List.fold_left
    (fun genv (symb, def) ->
      let loc = location_of_symbol symb in
      add_def genv symb loc def)
    empty defs

let find_def genv loc = PMap.find loc genv.defs

let serialize_def def =
  let open Gil_syntax in
  let lit =
    match def with
    | FunDef fname -> Literal.LList [ String "function"; String fname ]
    | GlobVar vname -> Literal.LList [ String "variable"; String vname ]
  in
  lit

let parse str =
  str |> String.trim |> String.split_on_char '\n'
  |> List.map (fun x -> x |> String.trim |> String.split_on_char ',')
  |> List.fold_left
       (fun genv quad ->
         match quad with
         | [ symbol; loc; "fun"; fun_def ] -> add_fundef genv symbol loc fun_def
         | [ symbol; loc; "var"; gvar ] -> add_globvar genv symbol loc gvar
         | _ -> failwith "invalid global environment")
       empty

let to_string genv =
  let lines =
    PMap.foldi
      (fun symb loc acc ->
        let def = PMap.find loc genv.defs in
        let kind, def =
          match def with
          | FunDef fdef -> ("fun", fdef)
          | GlobVar gvar -> ("var", gvar)
        in
        Printf.sprintf "%s,%s,%s,%s" symb loc kind def :: acc)
      genv.symb []
  in
  String.concat "\n" lines
