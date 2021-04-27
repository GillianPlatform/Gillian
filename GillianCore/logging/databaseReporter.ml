(**
    Reporter which logs to a database
*)

let db_name = "log.db"

let db = ref None

exception Error of string

let error fmt = Format.kasprintf (fun err -> raise (Error err)) fmt

let get_db () =
  match !db with
  | None    -> error "No Database"
  | Some db -> db

let check_result_code db ~log rc =
  match (rc : Sqlite3.Rc.t) with
  | OK | DONE | ROW -> ()
  | _ as err        ->
      error "%s: %s (%s)" log (Sqlite3.Rc.to_string err) (Sqlite3.errmsg db)

let exec db ~log ~stmt =
  let rc = Sqlite3.exec db stmt in
  try check_result_code db ~log rc
  with Error err -> error "exec: %s (%s)" err (Sqlite3.errmsg db)

let create_report_table db =
  exec db ~log:"creating report table"
    ~stmt:
      "CREATE TABLE report ( id TEXT PRIMARY KEY, title TEXT NOT NULL, \
       elapsed_time REAL NOT NULL, previous TEXT, parent TEXT, content TEXT \
       NOT NULL, severity INT NOT NULL, type TEXT NOT NULL);"

let create_db () =
  let new_db = Sqlite3.db_open db_name in
  (* Set synchronous to OFF for performance *)
  exec new_db ~log:"synchronous=OFF" ~stmt:"PRAGMA synchronous=OFF";
  create_report_table new_db;
  db := Some new_db

let reset_db () = if Sys.file_exists db_name then Sys.remove db_name else ()

let store_report (report : Report.t) db =
  let stmt =
    Sqlite3.prepare db "INSERT INTO report VALUES (?, ?, ?, ?, ?, ?, ?, ?);"
  in
  Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT (Uuidm.to_string report.id))
  |> check_result_code db ~log:"report bind id";
  Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT report.title)
  |> check_result_code db ~log:"report bind title";
  Sqlite3.bind stmt 3 (Sqlite3.Data.FLOAT report.elapsed_time)
  |> check_result_code db ~log:"report bind elapsed time";
  Sqlite3.bind stmt 4
    (Sqlite3.Data.opt_text (Option.map Uuidm.to_string report.previous))
  |> check_result_code db ~log:"report bind previous";
  Sqlite3.bind stmt 5
    (Sqlite3.Data.opt_text (Option.map Uuidm.to_string report.parent))
  |> check_result_code db ~log:"report bind parent";
  Sqlite3.bind stmt 6
    (Sqlite3.Data.TEXT
       (Yojson.Safe.to_string (Loggable.loggable_to_yojson report.content)))
  |> check_result_code db ~log:"report bind content";
  Sqlite3.bind stmt 7
    (Sqlite3.Data.INT (Int64.of_int (Report.severity_to_enum report.severity)))
  |> check_result_code db ~log:"report bind severity";
  Sqlite3.bind stmt 8 (Sqlite3.Data.TEXT report.type_)
  |> check_result_code db ~log:"report bind type";
  Sqlite3.step stmt |> check_result_code db ~log:"step: store report";
  Sqlite3.finalize stmt |> check_result_code db ~log:"finalize: store report"

let initialize () =
  reset_db ();
  create_db ()

let log (report : Report.t) = store_report report (get_db ())

let wrap_up () =
  match !db with
  | None    -> ()
  | Some db ->
      if not (Sqlite3.db_close db) then
        error "closing: %s (%s)"
          (Sqlite3.errcode db |> Sqlite3.Rc.to_string)
          (Sqlite3.errmsg db)
