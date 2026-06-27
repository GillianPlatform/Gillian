(** Reporter which logs to a database *)

let initialize () =
  Log_database.reset_db ();
  Log_database.create_db ()

let will_log (type_ : string) =
  not Logging_constants.Content_type.(type_ = debug || type_ = phase)

let log (report : Report.t) =
  if will_log report.type_ then
    let id = Sqlite3.Data.INT report.id in
    let title = Sqlite3.Data.TEXT report.title in
    let elapsed_time = Sqlite3.Data.FLOAT report.elapsed_time in
    let previous = Sqlite3.Data.opt_int64 report.previous in
    let parent = Sqlite3.Data.opt_int64 report.parent in
    let content =
      Sqlite3.Data.TEXT
        (Yojson.Safe.to_string (Loggable.loggable_to_yojson report.content))
    in
    let severity =
      Sqlite3.Data.INT
        (Int64.of_int (Logging_constants.Severity.to_enum report.severity))
    in
    let type_ = Sqlite3.Data.TEXT report.type_ in

    Log_database.store_report ~id ~title ~elapsed_time ~previous ~parent
      ~content ~severity ~type_

let wrap_up () = Log_database.close_db ()
