exception Unexpected_irep of Irep.t option * string
exception Code_error of Irep.t option * string

let () =
  Printexc.register_printer (function
    | Unexpected_irep (irep, msg) ->
        let json =
          match irep with
          | None -> ""
          | Some irep -> Irep.to_yojson irep |> Yojson.Safe.pretty_to_string
        in
        Some (Fmt.str "Unexpected Irep:\n%s\n\n%s" msg json)
    | Code_error (irep, msg) ->
        let json =
          match irep with
          | None -> ""
          | Some irep ->
              Fmt.str "\n\nHappened while handling this irep:\n%a"
                (Yojson.Safe.pretty_print ~std:false)
                (Irep.to_yojson irep)
        in
        Some (Fmt.str "There seem to be a bug in Kanillain: %s.%s" msg json)
    | _ -> None)

let unexpected ?irep msg = raise (Unexpected_irep (irep, msg))
let code_error ?irep msg = raise (Code_error (irep, msg))
