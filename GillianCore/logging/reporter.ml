type t = < log : 'a. 'a Report.t -> unit ; wrap_up : unit -> unit >

class file_reporter () =
  let out_channel = open_out "out.log" in
  let formatter = Format.formatter_of_out_channel out_channel in
  object
    val formatter = formatter

    val out_channel = out_channel

    method log : 'a. 'a Report.t -> unit =
      fun report ->
        match report.content with
        | Debug msgf  ->
            msgf @@ fun fmt -> Format.fprintf formatter @@ fmt ^^ "@,@?"
        | Phase phase ->
            Format.fprintf formatter "*** Phase %s ***@,@?"
            @@ Report.string_of_phase phase

    method wrap_up () = close_out out_channel
  end

class database_reporter () =
  object (self)
    val database =
      if Sys.file_exists "db.log" then Sys.remove "db.log";
      Sanddb.create_json_database "db.log" (module Report_j)

    method private serialize_content : 'a. 'a Report.content -> string =
      function
      | Debug msgf  ->
          let str = ref "" in
          (msgf @@ fun fmt -> Format.kasprintf (fun s -> str := s) fmt);
          !str
      | Phase phase ->
          Format.asprintf "Phase %s" @@ Report.string_of_phase phase

    method private serialize_severity : Report.severity -> Report_t.severity =
      function
      | Info    -> `Info
      | Log     -> `Log
      | Success -> `Success
      | Error   -> `Error
      | Warning -> `Warning

    method log : 'a. 'a Report.t -> unit =
      fun report ->
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

    method wrap_up () = ()
  end

let reporters : t list ref =
  ref [ new file_reporter (); new database_reporter () ]

let log report =
  List.iter (fun (reporter : t) -> reporter#log report) !reporters

let wrap_up () = List.iter (fun reporter -> reporter#wrap_up ()) !reporters
