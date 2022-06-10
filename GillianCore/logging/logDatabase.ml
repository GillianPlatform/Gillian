let db_name = "log.db"
let db = ref None

exception Error of string

let error fmt = Format.kasprintf (fun err -> raise (Error err)) fmt
let is_enabled () = Option.is_some !db

let get_db () =
  match !db with
  | None ->
      error
        "Unable to get database. Ensure that LogDatabase.create_db has been \
         called."
  | Some db -> db

let check_result_code db ~log rc =
  match (rc : Sqlite3.Rc.t) with
  | OK | DONE | ROW -> ()
  | _ as err ->
      error "%s: %s (%s)" log (Sqlite3.Rc.to_string err) (Sqlite3.errmsg db)

let exec db ~log ~stmt =
  let rc = Sqlite3.exec db stmt in
  try check_result_code db ~log rc
  with Error err -> error "exec: %s (%s)" err (Sqlite3.errmsg db)

let zero_or_one_row db ~log ~stmt =
  match Sqlite3.step stmt with
  | ROW -> (
      let row = Sqlite3.row_data stmt in
      match Sqlite3.step stmt with
      | DONE -> Some row
      | ROW -> error "%s: expected zero or one row, got more than one row" log
      | err ->
          error "%s: %s (%s)" log (Sqlite3.Rc.to_string err) (Sqlite3.errmsg db)
      )
  | DONE -> None
  | err ->
      error "%s: %s (%s)" log (Sqlite3.Rc.to_string err) (Sqlite3.errmsg db)

let create_report_table db =
  exec db ~log:"creating report table"
    ~stmt:
      "CREATE TABLE report ( id INTEGER PRIMARY KEY, title TEXT NOT NULL, \
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
  | None -> ()
  | Some db ->
      if not (Sqlite3.db_close db) then
        error "closing: %s (%s)"
          (Sqlite3.errcode db |> Sqlite3.Rc.to_string)
          (Sqlite3.errmsg db)

let store_report
    ~id
    ~title
    ~elapsed_time
    ~previous
    ~parent
    ~content
    ~severity
    ~type_ =
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
  Sqlite3.bind stmt 1 (Sqlite3.Data.INT id)
  |> check_result_code db ~log:"get report bind id";
  let row = zero_or_one_row db ~log:"step: get next report" ~stmt in
  let report_fields =
    Option.map
      (fun row ->
        (Sqlite3.Data.to_string_exn row.(0), Sqlite3.Data.to_string_exn row.(1)))
      row
  in
  Sqlite3.finalize stmt |> check_result_code db ~log:"finalize: get report";
  report_fields

let get_previous_report_id id =
  let db = get_db () in
  let stmt = Sqlite3.prepare db "SELECT previous FROM report WHERE id = ?;" in
  Sqlite3.bind stmt 1 (Sqlite3.Data.INT id)
  |> check_result_code db ~log:"get previous report bind id";
  let row = zero_or_one_row db ~log:"step: get previous report" ~stmt in
  let prev_report_id =
    Option.bind row (fun row ->
        Int64.of_string_opt @@ Sqlite3.Data.to_string_exn row.(0))
  in
  Sqlite3.finalize stmt
  |> check_result_code db ~log:"finalize: get previous report";
  prev_report_id

let get_next_reports id =
  let db = get_db () in
  let stmt =
    Sqlite3.prepare db "SELECT id, type, content FROM report WHERE previous=?;"
  in
  Sqlite3.bind stmt 1 (Sqlite3.Data.INT id)
  |> check_result_code db ~log:"get nexts bind id";
  let rc, children =
    Sqlite3.fold stmt
      ~f:(fun results row ->
        let id : ReportId.t = Sqlite3.Data.to_int64_exn row.(0) in
        let type_ = Sqlite3.Data.to_string_exn row.(1) in
        let content = Sqlite3.Data.to_string_exn row.(2) in
        (id, type_, content) :: results)
      ~init:[]
  in
  rc |> check_result_code db ~log:"fold: get nexts";
  children

let get_previously_freed_annot loc =
  let db = get_db () in
  let stmt =
    Sqlite3.prepare db
      "SELECT json_extract(content, '$.annot') FROM report WHERE  type=? AND \
       parent=(SELECT parent from report WHERE type=? AND \
       json_extract(content, '$.loc')=? ORDER BY elapsed_time DESC LIMIT 1) \
       ORDER BY elapsed_time DESC LIMIT 1;"
  in
  Sqlite3.bind stmt 1
    (Sqlite3.Data.TEXT LoggingConstants.ContentType.annotated_action)
  |> check_result_code db ~log:"get previous freed annot bind annotated_action";
  Sqlite3.bind stmt 2
    (Sqlite3.Data.TEXT LoggingConstants.ContentType.set_freed_info)
  |> check_result_code db ~log:"get previous freed annot bind set_freed_info";
  Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT loc)
  |> check_result_code db ~log:"get previous freed annot bind loc";
  let row = zero_or_one_row db ~log:"step: get previous freed annot" ~stmt in
  let annot = Option.map (fun row -> Sqlite3.Data.to_string_exn row.(0)) row in
  Sqlite3.finalize stmt
  |> check_result_code db ~log:"finalize: get previous freed annot";
  annot

let get_children_of roots_only id =
  let db = get_db () in
  let query =
    Fmt.str "SELECT id, type, content FROM report WHERE parent=?%s;"
      (if roots_only then " AND previous IS NULL" else "")
  in
  let stmt = Sqlite3.prepare db query in
  Sqlite3.bind stmt 1 (Sqlite3.Data.INT id)
  |> check_result_code db ~log:"get children bind id";
  let rc, children =
    Sqlite3.fold stmt
      ~f:(fun results row ->
        let id : ReportId.t = Sqlite3.Data.to_int64_exn row.(0) in
        let type_ = Sqlite3.Data.to_string_exn row.(1) in
        let content = Sqlite3.Data.to_string_exn row.(2) in
        (id, type_, content) :: results)
      ~init:[]
  in
  rc |> check_result_code db ~log:"fold: get children";
  children
