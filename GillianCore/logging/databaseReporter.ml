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

class virtual ['a] t =
  object (self)
    method log_agnostic : 'a. (Report.agnostic, 'a) Report.t -> unit =
      fun report ->
        if enabled () then
          self#insert_report report
          @@ self#serialize_agnostic_content report.content

    method log_specific (report : (Report.specific, 'a) Report.t) =
      if enabled () then
        self#insert_report report
        @@ self#serialize_specific_content report.content

    method private insert_report : 'a 'b. ('a, 'b) Report.t -> string -> unit =
      fun report content ->
        let report : Report_t.t =
          {
            id = Uuidm.to_string report.id;
            title = report.title;
            elapsed_time = report.elapsed_time;
            previous = Option.map Uuidm.to_string report.previous;
            parent = Option.map Uuidm.to_string report.parent;
            content;
            severity = self#serialize_severity report.severity;
          }
        in
        ignore @@ Sanddb.insert_record self#database report

    method private serialize_severity : Report.severity -> Report_t.severity =
      function
      | Info    -> `Info
      | Log     -> `Log
      | Success -> `Success
      | Error   -> `Error
      | Warning -> `Warning

    method private database = get_database ()

    method private serialize_agnostic_content
        : 'a. (Report.agnostic, 'a) Report.content -> string =
      function
      | Debug msgf  -> Report.PackedPP.str msgf
      | Phase phase ->
          Format.asprintf "Phase %s" @@ Report.string_of_phase phase

    method private serialize_specific_content
        : (Report.specific, 'a) Report.content -> string =
      function
      | TargetLang tl -> self#serialize_target_lang tl

    method virtual private serialize_target_lang : 'a -> string

    method wrap_up = wrap_up ()
  end
