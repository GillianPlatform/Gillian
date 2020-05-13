type t = { log : Report.t -> unit; wrap_up : unit -> unit }

let file_reporter () =
  let out_channel = open_out "out.log" in
  let formatter = Format.formatter_of_out_channel out_channel in
  let log (report : Report.t) =
    match report.content with
    | Debug msgf  ->
        Report.PackedPP.pf formatter msgf;
        Format.fprintf formatter "@,@?"
    | Phase phase ->
        Format.fprintf formatter "*** Phase %s ***@,@?"
        @@ Report.string_of_phase phase
  in
  let wrap_up () = close_out out_channel in
  { log; wrap_up }

let database_reporter () =
  let () = if Sys.file_exists "db.log" then Sys.remove "db.log" in
  let database = Sanddb.create_json_database "db.log" (module Report_j) in
  let serialize_content : Report.content -> string = function
    | Debug msgf  -> Report.PackedPP.str msgf
    | Phase phase -> Format.asprintf "Phase %s" @@ Report.string_of_phase phase
  in
  let serialize_severity : Report.severity -> Report_t.severity = function
    | Info    -> `Info
    | Log     -> `Log
    | Success -> `Success
    | Error   -> `Error
    | Warning -> `Warning
  in
  let log (report : Report.t) =
    let report : Report_t.t =
      {
        id = Uuidm.to_string report.id;
        title = report.title;
        elapsed_time = report.elapsed_time;
        previous = Option.map Uuidm.to_string report.previous;
        parent = Option.map Uuidm.to_string report.parent;
        content = serialize_content report.content;
        severity = serialize_severity report.severity;
      }
    in
    let _ = Sanddb.insert_record database report in
    ()
  in
  let wrap_up () = () in
  { log; wrap_up }

let reporters = ref [ file_reporter (); database_reporter () ]

let log report = List.iter (fun reporter -> reporter.log report) !reporters

let wrap_up () = List.iter (fun reporter -> reporter.wrap_up ()) !reporters
