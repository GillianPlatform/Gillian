class t () =
  let () = if Sys.file_exists "db.log" then Sys.remove "db.log" in
  let database = Sanddb.create_json_database "db.log" (module Report_j) in
  object (self)
    method private serialize_content : Report.content -> string =
      function
      | Debug msgf  -> Report.PackedPP.str msgf
      | Phase phase ->
          Format.asprintf "Phase %s" @@ Report.string_of_phase phase

    method private serialize_severity : Report.severity -> Report_t.severity =
      function
      | Info    -> `Info
      | Log     -> `Log
      | Success -> `Success
      | Error   -> `Error
      | Warning -> `Warning

    method log (report : Report.t) =
      let report : Report_t.t =
        {
          id = Uuidm.to_string report.id;
          title = report.title;
          elapsed_time = report.elapsed_time;
          previous = Option.map Uuidm.to_string report.previous;
          parent = Option.map Uuidm.to_string report.parent;
          content = self#serialize_content report.content;
          severity = self#serialize_severity report.severity;
        }
      in
      let _ = Sanddb.insert_record database report in
      ()

    method wrap_up = ()
  end
