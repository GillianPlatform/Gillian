type t = (string, Symbol.t) Hashtbl.t

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  let open Utils.Syntaxes.Result in
  let* symbols =
    match json with
    | `Assoc symbols -> Ok symbols
    | _ -> Error "Invalid symtab.json file"
  in
  let tbl = Hashtbl.create 1000 in
  let+ () =
    let cur_name = ref "" in
    try
      Ok
        (List.iter
           (fun (name, sym) ->
             cur_name := name;
             Hashtbl.replace tbl name (Symbol.of_yojson sym))
           symbols)
    with Kutils.J.Parse_error (_, s) ->
      Error ("When parsing symbol " ^ !cur_name ^ ": " ^ s)
  in
  tbl
