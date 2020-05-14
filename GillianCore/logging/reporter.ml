type t = < log : Report.t -> unit ; wrap_up : unit >

class file_reporter () =
  let out_channel = open_out "out.log" in
  let formatter = Format.formatter_of_out_channel out_channel in
  object
    method log (report : Report.t) =
      match report.content with
      | Debug msgf  ->
          Report.PackedPP.pf formatter msgf;
          Format.fprintf formatter "@,@?"
      | Phase phase ->
          Format.fprintf formatter "*** Phase %s ***@,@?"
          @@ Report.string_of_phase phase

    method wrap_up = close_out out_channel
  end

class database_reporter () =
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

let reporters =
  ref [ (new file_reporter () :> t); (new database_reporter () :> t) ]

let log report = List.iter (fun reporter -> reporter#log report) !reporters

let wrap_up () = List.iter (fun reporter -> reporter#wrap_up) !reporters
