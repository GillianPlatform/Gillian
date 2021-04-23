let filename = "file.log"

let is_enabled = ref false

let out_channel = ref None

let formatter = ref None

let enable () = is_enabled := true

let initialize () =
  if !is_enabled then
    let () = out_channel := Some (open_out filename) in
    formatter :=
      Some (Format.formatter_of_out_channel (Option.get !out_channel))

let log (report : Report.t) : unit =
  if !is_enabled then
    match !formatter with
    | None           -> ()
    | Some formatter -> (
        match report.type_ with
        | type_
          when type_ = LoggingConstants.ContentType.debug
               || type_ = LoggingConstants.ContentType.phase ->
            let () = Loggable.pp report.content formatter in
            Format.fprintf formatter "@,@?"
        | _ -> ())

let wrap_up () =
  if !is_enabled then
    match !out_channel with
    | None             -> ()
    | Some out_channel -> close_out out_channel
