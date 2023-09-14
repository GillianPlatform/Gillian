type t = (string, Symbol.t) Hashtbl.t

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  let open Utils.Syntaxes.Result in
  let* symbols =
    match json with
    | `Assoc [ ("symbolTable", `Assoc symbols) ] -> Ok symbols
    | _ -> Error "Invalid symtab.json file"
  in
  let tbl = Hashtbl.create 1000 in
  let+ () =
    try
      Ok
        (List.iter
           (fun (name, sym) -> Hashtbl.replace tbl name (Symbol.of_yojson sym))
           symbols)
    with Kutils.J.Parse_error (_, s) -> Error s
  in
  tbl
