(**
    Reporter which logs to a file
*)

let filename = "file.log"
let out_channel = ref None
let formatter = ref None

let initialize () =
  let () = out_channel := Some (open_out filename) in
  formatter := Some (Format.formatter_of_out_channel (Option.get !out_channel))

let log (report : Report.t) : unit =
  match !formatter with
  | None -> ()
  | Some formatter -> (
      (* TODO: This should eventually log all types when all regular calls to
               log of specific types are replaced *)
      match report.type_ with
      | type_
        when type_ = LoggingConstants.ContentType.debug
             || type_ = LoggingConstants.ContentType.assertion
             || type_ = LoggingConstants.ContentType.phase ->
          let () = Loggable.pp report.content formatter in
          Format.fprintf formatter "@,@?"
      | _ -> ())

let wrap_up () =
  match !out_channel with
  | None -> ()
  | Some out_channel -> close_out out_channel
