type err_t = Symbol_not_found of string
type def = FunDef of string | GlobVar of string

type t = {
  symb : (string, string) PMap.t;  (** maps symbols to loc names *)
  defs : (string, def) PMap.t;  (** maps loc names to definitions *)
}

let add_fundef genv symb loc fdef =
  let symb = PMap.add symb loc genv.symb in
  let defs = PMap.add loc (FunDef fdef) genv.defs in
  { symb; defs }

let add_globvar genv symb loc gvar =
  let symb = PMap.add symb loc genv.symb in
  let defs = PMap.add loc (GlobVar gvar) genv.defs in
  { symb; defs }

let empty = { symb = PMap.empty; defs = PMap.empty }
let find_opt x s = try Some (PMap.find x s) with Not_found -> None

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
