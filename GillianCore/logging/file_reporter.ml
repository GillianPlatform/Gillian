(**
    Reporter which logs to a file
*)

let filename = "file.log"
let out_channel = ref None
let formatter = ref None

let accepted_types =
  Logging_constants.Content_type.
    [ debug; assertion; phase; cmd; unify; unify_result ]

let initialize () =
  let () = out_channel := Some (open_out filename) in
  formatter := Some (Format.formatter_of_out_channel (Option.get !out_channel))

let will_log (type_ : string) = List.mem type_ accepted_types

let log (report : Report.t) : unit =
  match !formatter with
  | None -> ()
  | Some formatter -> (
      (* TODO: This should eventually log all types when all regular calls to
               log of specific types are replaced *)
      match report.type_ with
      | type_ when will_log type_ ->
          let () = Loggable.pp report.content formatter in
          Format.fprintf formatter "@\n@?"
      | _ -> ())

let wrap_up () =
  match !out_channel with
  | None -> ()
  | Some out_channel -> close_out out_channel
