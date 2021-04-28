let db_name = "log.db"

let db = ref None

exception Error of string

let error fmt = Format.kasprintf (fun err -> raise (Error err)) fmt

let get_db () =
  match !db with
  | None    ->
      error
        "Unable to get database. Ensure that LogDatabase.create_db has been \
         called."
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

let close_db () =
  match !db with
  | None    -> ()
  | Some db ->
      if not (Sqlite3.db_close db) then
        error "closing: %s (%s)"
          (Sqlite3.errcode db |> Sqlite3.Rc.to_string)
          (Sqlite3.errmsg db)

let store_report
    ~id ~title ~elapsed_time ~previous ~parent ~content ~severity ~type_ =
  let db = get_db () in
  let stmt =
    Sqlite3.prepare db "INSERT INTO report VALUES (?, ?, ?, ?, ?, ?, ?, ?);"
  in
  Sqlite3.bind stmt 1 id |> check_result_code db ~log:"store report bind id";
  Sqlite3.bind stmt 2 title
  |> check_result_code db ~log:"store report bind title";
  Sqlite3.bind stmt 3 elapsed_time
  |> check_result_code db ~log:"store report bind elapsed time";
  Sqlite3.bind stmt 4 previous
  |> check_result_code db ~log:"store report bind previous";
  Sqlite3.bind stmt 5 parent
  |> check_result_code db ~log:"store report bind parent";
  Sqlite3.bind stmt 6 content
  |> check_result_code db ~log:"store report bind content";
  Sqlite3.bind stmt 7 severity
  |> check_result_code db ~log:"store report bind severity";
  Sqlite3.bind stmt 8 type_
  |> check_result_code db ~log:"store report bind type";
  Sqlite3.step stmt |> check_result_code db ~log:"step: store report";
  Sqlite3.finalize stmt |> check_result_code db ~log:"finalize: store report"

let get_report id =
  let db = get_db () in
  let stmt =
    Sqlite3.prepare db "SELECT content, type FROM report WHERE id=?;"
  in
  Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id)
  |> check_result_code db ~log:"get report bind id";
  let rc = Sqlite3.step stmt in
  let report_fields =
    if rc == Sqlite3.Rc.ROW then
      let rows = Sqlite3.row_blobs stmt in
      (rows.(0), rows.(1))
    else error "step: (%s)" (Sqlite3.errcode db |> Sqlite3.Rc.to_string)
  in
  Sqlite3.finalize stmt |> check_result_code db ~log:"finalize: get report";
  report_fields

let get_previous_report_id id =
  let db = get_db () in
  let stmt =
    Sqlite3.prepare db "SELECT id FROM report WHERE elapsed_time < (SELECT elapsed_time FROM report WHERE id=?) AND type='cmd_step' ORDER BY elapsed_time DESC LIMIT 1;"
  in
  Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id)
  |> check_result_code db ~log:"get report bind id";
  let rc = Sqlite3.step stmt in
  let prev_report_id =
    if rc == Sqlite3.Rc.ROW then
      let rows = Sqlite3.row_blobs stmt in
      if Array.length rows = 0 then
        None
      else
        Some rows.(0)
    else error "step: (%s)" (Sqlite3.errcode db |> Sqlite3.Rc.to_string)
  in
  Sqlite3.finalize stmt |> check_result_code db ~log:"finalize: get report";
  prev_report_id