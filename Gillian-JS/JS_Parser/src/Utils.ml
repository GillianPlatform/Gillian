open Batteries.Incubator

let search_string_forward regex str : int option =
  try Some (Str.search_forward regex str 0) with Not_found -> None

let check_parsing_errors errors =
  match errors with
  | []     -> ()
  | errors ->
      let pretty_messages =
        List.map
          (fun (loc, err) ->
            let loc_str = Flow_parser.Loc.show loc in
            let err_str = Flow_parser.Parse_error.PP.error err in
            Printf.sprintf "%s: %s" loc_str err_str)
          errors
      in
      let messages = String.concat "\n" pretty_messages in
      let error_type =
        match
          search_string_forward
            (Str.regexp "Invalid left-hand side in assignment")
            (List.hd pretty_messages)
        with
        | Some _ -> "ReferenceError"
        | None   -> "SyntaxError"
      in
      raise (Error.ParserError (Error.FlowParser (messages, error_type)))

module Path = PathGen.OfString

let normalize_path path_str =
  let path = Path.of_string path_str in
  let normalized_path = Path.normalize_in_tree path in
  Path.to_string normalized_path

let begins_with str prefix =
  let str_len = String.length str in
  let prefix_len = String.length prefix in
  prefix_len <= str_len && String.sub str 0 prefix_len = prefix

let load_file path : string =
  let in_chan = open_in path in
  let chan_len = in_channel_length in_chan in
  let buffer = Bytes.create chan_len in
  really_input in_chan buffer 0 chan_len;
  close_in in_chan;
  Bytes.to_string buffer

let opt_map f (x : 'a option) =
  match x with
  | None   -> (None, [])
  | Some v ->
      let a, bs = f v in
      (Some a, bs)

let map f (xs : 'a list) =
  List.fold_left
    (fun (acc_a, acc_b) x ->
      let a, bs = f x in
      (acc_a @ [ a ], acc_b @ bs))
    ([], []) xs

module Str_set = Set.Make (String)

let fresh_sth (name : string) : (unit -> string) * (unit -> unit) =
  let counter = ref 0 in
  let f () =
    let v = name ^ string_of_int !counter in
    counter := !counter + 1;
    v
  in
  let r () = counter := 0 in
  (f, r)

let fresh_aux_var, reset_aux_var = fresh_sth "aux_var_"
