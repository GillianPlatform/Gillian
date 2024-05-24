(** Strips results from an other-imports mapping, raising an exception in the [Error] case *)
let convert_other_imports oi =
  List.map
    (fun (ext, f) ->
      let fun_with_exn s = Stdlib.Result.get_ok (f s) in
      (ext, fun_with_exn))
    oi

(** Writes a GIL program to the specified filename *)
let burn_gil ~(init_data : Yojson.Safe.t) ~pp_prog prog outfile_opt =
  match outfile_opt with
  | Some outfile ->
      let outc = open_out outfile in
      let fmt = Format.formatter_of_out_channel outc in
      let () =
        match init_data with
        | `Null -> ()
        | init_data ->
            Fmt.pf fmt "#begin_init_data@\n%a@\n#end_init_data@\n"
              (Yojson.Safe.pretty_print ~std:false)
              init_data
      in
      let () = pp_prog fmt prog in
      close_out outc
  | None -> ()
