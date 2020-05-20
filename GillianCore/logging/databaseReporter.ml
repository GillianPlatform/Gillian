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
    method log (report : 'a Report.t) =
      if enabled () then
        match report.content with
        | Agnostic c -> self#serialize_agnostic c |> self#insert_report report
        | Specific c -> self#serialize_specific c |> self#insert_report report

    method wrap_up = wrap_up ()

    method private serialize_agnostic =
      function
      | Debug msgf  -> Report.PackedPP.str msgf
      | Phase phase ->
          Format.asprintf "Phase %s" @@ Report.string_of_phase phase

    method virtual private serialize_specific : 'a -> string

    method private insert_report report content =
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
  end

let default : type a. unit -> a t =
 fun () ->
  object
    inherit [a] t

    method private serialize_specific _ = ""
  end
