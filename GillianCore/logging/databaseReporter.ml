module Types = struct
  type conf = { filename : string }

  type state = {
    database : (module Sanddb.Database.T with type t = Report_j.t);
  }
end

include Reporter.Make (struct
  include Types

  let enabled = false

  let conf = { filename = "database.log" }

  let initialize { filename; _ } =
    if Sys.file_exists filename then Sys.remove "database.log";
    { database = Sanddb.create_json_database filename (module Report_j) }

  let wrap_up _ = ()
end)

let get_database () = (get_state ()).database

class t =
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
      if enabled () then
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
        let _ = Sanddb.insert_record self#database report in
        ()

    method private database = get_database ()

    method wrap_up = wrap_up ()
  end
